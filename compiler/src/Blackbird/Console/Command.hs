{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Console.Command
  ( Command(..)
  , HasCommand(..)
  , checkAndCompile
  , commands
  , executeCommand
  ) where

import Bound
import Bound.Var (unvar)
import Control.Lens
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class
import Control.Monad.State (StateT (..), evalStateT)
import Data.Bifunctor
import Data.Bitraversable
import Data.Char
import Data.Default
import Data.Foldable (for_)
import Data.List as List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split (splitOn)
import Data.Set (notMember)
import Data.Set.Lens
import Data.Text (Text, pack)
import Data.Traversable (for)
import Data.Void
import Prettyprinter.Render.Terminal (bold)
import System.Console.Haskeline
import System.Exit
import Text.Groom
import Text.Parser.Combinators (eof)
import Text.Parser.Token (semiSep1)
import Text.Trifecta.Result

import Blackbird.Builtin (parserState, predefs, primOps, resolveGlobals)
import Blackbird.Console.Module
import Blackbird.Console.State
import Blackbird.Constraint.Env
import Blackbird.Core.Compiler
import Blackbird.Core.Optimizer
import Blackbird.Inference.Kind as Kind
import Blackbird.Inference.Type as Type
import Blackbird.Interpreter as Interp
import Blackbird.Parser.Data
import Blackbird.Parser.Kind
import Blackbird.Parser.Term
import Blackbird.Parser.Trifecta (Parser, parseString)
import Blackbird.Parser.Type
import Blackbird.Pretty
import Blackbird.Pretty.Core
import Blackbird.Pretty.G
import Blackbird.Pretty.Kind
import Blackbird.Pretty.Term
import Blackbird.Pretty.Type
import Blackbird.Syntax
import Blackbird.Syntax.Convention
import Blackbird.Syntax.Core as Core
import Blackbird.Syntax.Data (DataType)
import Blackbird.Syntax.G (G)
import Blackbird.Syntax.Kind as Kind
import Blackbird.Syntax.Name
import Blackbird.Syntax.Scope
import Blackbird.Syntax.Term as Term
import Blackbird.Syntax.Type as Type
import Blackbird.Unification.Kind
import Blackbird.Unification.Meta
import Blackbird.Version

------------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------------

data Command = Command
  { _cmdName :: String
  , _alts    :: [String]
  , _arg     :: Maybe String
  , _tabbed  :: Maybe (CompletionFunc Console)
  , _desc    :: String
  , _body    :: [String] -> String -> Console ()
  }

makeClassy ''Command

cmd :: String -> Command
cmd nm = Command nm [] Nothing Nothing "" $ \_ _ -> return ()

getCommand :: String -> Maybe (Command, [String], String)
getCommand zs = commands ^?
    folded.
    filtered (\c -> isPrefixOf xs (c^.cmdName)
                 || anyOf (alts.folded) (isPrefixOf xs) c).
    to (,as,ys')
  where
    (cs, ys) = break isSpace zs
    xs:as = splitOn "+" cs -- TODO is there a NonEmpty version?
    ys' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace ys

executeCommand :: String -> Console ()
executeCommand txt = case getCommand txt of
  Just (c,args,input)  -> view body c args input
  Nothing          -> do
    sayLn $ "blackbird: error: Unknown command:" <+> pretty (show txt)
    showHelp [] txt

showHelp :: [String] -> String -> Console ()
showHelp _ _ = sayLn $ vsep (map format commands) where
  format c = fill 18 (withArg c) <+> hang 18 (fillSep (pretty <$> words (c^.desc)))
  withArg c = case c^.arg of
    Nothing -> annotate bold (pretty ':' <> pretty (c^.cmdName))
    Just a  -> annotate bold (pretty ':' <> pretty (c^.cmdName)) <+> angles (pretty a)

------------------------------------------------------------------------------
-- commands
------------------------------------------------------------------------------

procArgs :: [String] -> NonEmpty (String, a) -> a
procArgs args (x :| xs) = foldl f (snd x) args
 where
 m = x:xs
 f r s | Just a <- lookup s m = a
       | otherwise            = r

parsing :: Parser a -> ([String] -> a -> Console ())
        -> [String] -> String -> Console ()
parsing p k args s = case parseString (p <* eof) mempty s of
  Success a   -> k args a
  Failure doc -> sayLn $ _errDoc doc

-- TODO generalize with parsing
parsingS :: StateT s Parser a -> ([String] -> a -> Console ())
        -> [String] -> String -> StateT s Console ()
parsingS p k args s = StateT $ \st ->
  let p' = runStateT p st
  in case parseString (p' <* eof) mempty s of
      Success (a, st') -> k args a <&> (,st')
      Failure doc      -> sayLn (_errDoc doc) <&> (,st)

kindBody :: [String] -> Type.Typ (Maybe Text) (Var Text Text) -> Console ()
kindBody args ty = do
  gk <- ioM mempty $ do
    tm <- kindVars (maybe (newMeta False Nothing) pure)
      <=< bimemoverse (traverse $ newMeta False . Just)
                      (fmap pure . newMeta False . unvar Just Just)
        $ ty
    k <- inferKind id tm
    generalize k
  disp gk
 where
 disp :: Schema Void -> Console ()
 disp = procArgs args $
         ("pretty", sayLn . flip prettySchema names . vacuous)
      :| [("ugly", sayLn . pretty . groom)]

dkindsBody :: [String] -> [DataType () Text] -> Console ()
dkindsBody _ dts = do
  ckdts <- ioM mempty (checkDataTypeKinds dts)
  for_ ckdts $ \ckdt ->
    sayLn $ pretty (ckdt^.name)
        <+> colon
        <+> prettySchema (vacuous $ schema ckdt) names

checkAndCompile :: (MonadConstraint (KindM s) s m, Show c)
                => Term Ann Text -> m (Either (Doc AnsiStyle) (Type.Typ t k, Core Convention c))
checkAndCompile syn = traverse resolveGlobals' (syn >>= predefs) `for` \syn' -> do
  tm <- bitraverse (memoverse
                          (\s -> newMeta False Nothing >>=
                                   flip newMeta (Just s) . pure))
                 pure
                 syn'
  w <- inferType 0 fst $ first (first absurd) tm
  over _2 (>>= snd) <$> generalizeType w
  where
    resolveGlobals' x = maybe (Left $ "[E005] Unknown word:" <+> pretty x) Right $
      resolveGlobals x

withCheckAndCompile :: (MonadIO m, Show c) => Term Ann Text -> (Type.Typ t k -> Core Convention c -> m ()) -> m ()
withCheckAndCompile syn f = ioM mempty (runCM (checkAndCompile syn) dummyConstraintEnv) >>= \case
  Right (ty, c) -> f ty c
  Left doc -> sayLn doc

typeBody :: [String] -> Term Ann Text -> Console ()
typeBody args syn = withCheckAndCompile syn $ \ty _ -> disp ty
 where
 disp :: Type.Typ String String -> Console ()
 disp = procArgs args $
         ("pretty", \ty -> sayLn $ prettyType ty names (-1))
      :| [("ugly", sayLn . pretty . groom)]

coreBody :: [String] -> Term Ann Text -> Console ()
coreBody args syn = withCheckAndCompile syn $ \_ c -> disp (opt c)
 where
 disp :: Core Convention (Doc AnsiStyle) -> Console ()
 disp = procArgs args $
         ("pretty", sayLn . runIdentity . prettyCore names (-1) (const.pure))
      :| [("ugly", sayLn . pretty . groom)]
 opt = procArgs args $ ("opt", optimize) :| [("noopt", id)]

gBody :: [String] -> Term Ann Text -> Console ()
gBody args syn = withCheckAndCompile syn $ \_ c -> disp $ compile 0 absurd (opt c)
 where
 disp :: G -> Console ()
 disp = procArgs args $
         ("pretty", sayLn . prettyG (pretty <$> names) defaultCxt)
      :| [("ugly", sayLn . pretty . groom)]
 opt = procArgs args $ ("opt", optimize) :| [("noopt", id)]

echoBody :: [String] -> String -> Console ()
echoBody args =
  case procArgs args $ ("term", "term") :| [("type", "type"), ("kind", "kind")] of
    "kind" -> parsing kind (\_ s -> disp $ Kind.general s Just) args
     where
     disp :: Schema String -> Console ()
     disp = procArgs args $
              ("pretty", sayLn . (prettySchema ?? names))
          :| [("ugly", liftIO . putStrLn . groom)]
    "type" -> parsing typ (const $ disp . Type.abstractAll Just (unvar Just Just)) args
     where
     disp :: (Scope Int (TK ()) String, ([Maybe Text], [Maybe Text])) -> Console ()
     disp = procArgs args $
              ("pretty", pt)
          :| [("ugly", liftIO . putStrLn . groom . fst)]
     pt (tsch, hs) = let stsch :: Scope Int (TK String) String
                         stsch = hoistScope (first ("?" <$)) tsch
                      in sayLn $ prettyTypeSchema stsch hs names
    _ {- "term" -} -> flip evalStateT parserState -- TODO thread new state through
                      . parsingS term (\_ tm -> disp tm) args
     where
     disp :: Term Ann Text -> Console ()
     disp = procArgs args $
              ("pretty", pt)
          :| [("ugly", liftIO . putStrLn . groom)]
     pt tm = prettyTerm
               tm names' (-1) (error "TODO: prettyAnn") (pure.pure.pretty)
         >>= sayLn
      where names' = filter ((`notMember` setOf traverse tm).pack) names

evalBody :: [String] -> Term Ann Text -> Console ()
evalBody args syn = withCheckAndCompile syn $ \_ c -> liftIO $ do
    ms <- defaultMachineState 512 =<< primOps
    void $ eval (compile 0 absurd . opt $ c) def (ms & trace .~ debug)
 where
 opt = procArgs args $ ("opt", optimize) :| [("noopt", id)]
 debug = procArgs args $ ("nodebug", const $ return ()) :| [("debug", putStrLn)]

commands :: [Command]
commands =
  [ cmd "help" & desc .~ "show help" & alts .~ ["?"] & body .~ showHelp
  , cmd "quit" & desc .~ "quit" & body.mapped .~ const (liftIO exitSuccess)
  , cmd "kind" & desc .~ "infer the kind of a type"
      & body .~ parsing typ kindBody
  , cmd "type" & desc .~ "infer the type of a term"
      & body .~ (\args s -> evalStateT (parsingS term typeBody args s)
                                       parserState)
  , cmd "core" & desc .~ "dump the core representation of a term after type checking."
      & body .~ (\args s -> evalStateT (parsingS term coreBody args s)
                                       parserState)
  , cmd "g"
      & desc .~ "dump the sub-core representation of a term after type checking."
      & body .~ (\args s -> evalStateT (parsingS term gBody args s)
                                       parserState)
  , cmd "dkinds"
      & desc .~ "determine the kinds of a series of data types"
      & body .~ parsing (semiSep1 dataType) dkindsBody
  , cmd "echo"
      & desc .~ "parse and print an expression at any level"
      & body .~ echoBody
  , cmd "eval"
      & desc .~ "type check and evaluate a term"
      & body .~ (\args s -> evalStateT (parsingS term evalBody args s)
                                       parserState)
  -- TODO: this doesn't actually import anything, but at least it tries to parse it.
  -- TODO: and currently, it only looks in stdlib/Prelude for files. that needs to be fixed.
  , cmd "import"
      & desc .~ "import module"
      & body .~ (\_ s -> liftIO $ preludeTestLoader s >>= print)
  -- , cmd "udata"
  --     & desc .~ "show the internal representation of a data declaration"
  --     & body .~ parsing dataType (liftIO . putStrLn . groom)
  -- , cmd "load" & arg  ?~ "filename" & desc .~ "load a file" & body .~ \xs -> liftIO $ putStrLn =<< readFile xs
  , cmd "version"
      & desc .~ "show the compiler version number"
      & body .~ \_ _ -> liftIO $ putStrLn version
  ]
