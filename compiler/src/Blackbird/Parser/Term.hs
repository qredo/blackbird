{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for terms
--------------------------------------------------------------------
module Blackbird.Parser.Term
  ( term
  , terms
  , termDeclClause
  , declClause
  , declarations
  , declarations'
  , letBlock
  -- utility
  , blocked
  ) where

import Control.Lens hiding (op)
import Control.Applicative
import Control.Comonad
import Control.Monad.State hiding (guard)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.List.NonEmpty (groupWith, NonEmpty((:|)))
import qualified Data.List.NonEmpty as N (unzip)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as Set hiding (map)
import Data.Foldable (foldrM)
import Data.Text (Text, unpack)
import Blackbird.Builtin.Pattern
import Blackbird.Builtin.Term
import Blackbird.Parser.Style
import Blackbird.Parser.Type as Type
import Blackbird.Parser.Literal
import Blackbird.Parser.Pattern
import Blackbird.Parser.Trifecta (LayoutParsing(layouted), braces, brackets, parens)
import Blackbird.Syntax
import Blackbird.Syntax.Global (Assoc(..), Fixity(..), fixityLevel)
import Blackbird.Syntax.Literal
import Blackbird.Syntax.Pattern
import Blackbird.Syntax.Term
import Blackbird.Syntax.Module (HasFixities(..), FixityDecl(..), HasFixityDecl(..))
import Text.Parser.Combinators
import Text.Parser.Expression (Operator, OperatorTable, buildExpressionParser)
import qualified Text.Parser.Expression as ParserExpr
import Text.Parser.Token (TokenParsing, colon, commaSep, reserve, semiSep, symbol)

type Tm = Term Ann Text

-- | Parse an atomic term
term0 :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
term0 = Var <$> termIdentifier
   <|> tweakLiteral <$> literal
   <|> parens (tup' <$> terms)
   <|> brackets (makeList <$> terms)
 where
 tweakLiteral i@(Integer _) = App (Var "fromIntegerToWord") . HardTerm . Lit $ i
 tweakLiteral l = HardTerm $ Lit l
 makeList [] = Var "[]"
 makeList (x:xs) = App (App (Var "::") x) $ makeList xs

term1 :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
term1 = match
    <|> (get >>= flip buildExpressionParser (foldl1 App <$> some term0)
                 . mkOperators)
    -- TODO: handle AppHash

-- | Take our list of fixity declarations in scope and make a
-- 'parsers' table of operators for it.
mkOperators :: forall a m. (HasFixities a, TokenParsing m)
            => a                -- ^ The source of [FixityDecl].
            -> OperatorTable m Tm -- ^ Operator index for expression parser.
mkOperators = fmap snd . M.toDescList . collectByFixity . view fixityDecls
  where collectByFixity :: [FixityDecl] -> M.Map Int [Operator m Tm]
        collectByFixity = foldr (M.unionWith mappend) M.empty
                          . catMaybes
                          . fmap (\fd -> (fixityLevel $ fd ^. fixityDeclFixity)
                                         <&> flip M.singleton (parserOperators fd))
        parserOperators :: FixityDecl -> [Operator m Tm]
        parserOperators fd = parserOp (fd ^. fixityDeclFixity)
                         <$> fd ^. fixityDeclNames

parserAssoc :: Assoc -> ParserExpr.Assoc
parserAssoc L = ParserExpr.AssocLeft
parserAssoc R = ParserExpr.AssocRight
parserAssoc N = ParserExpr.AssocNone

parserOp :: TokenParsing m => Fixity -> Text -> Operator m Tm
parserOp (Infix a _) n = ParserExpr.Infix (App . App (Var n) <$ symbol (unpack n)) (parserAssoc a)
parserOp (Prefix _) n = ParserExpr.Prefix (App (Var n) <$ symbol (unpack n))
parserOp (Postfix _) n = ParserExpr.Postfix (App (Var n) <$ symbol (unpack n))
parserOp Idfix _ = error "parserOp: impossible"

sig :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
sig = (maybe id (Sig ??) ??) <$> term1 <*> optional (colon *> annotation)

branch :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m (Alt Ann (Term Ann) Text)
branch = do pp <- pattern
            g <- join (guarded (reserve op "->"))
            validate pp $ \n ->
                unexpected $ "duplicate bindings in pattern for: " ++ unpack n
            return $ alt pp g

blocked :: LayoutParsing m => m a -> m a
blocked x = braces x <|> colon *> layouted x

match :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
match = Case <$ symbol "case" <*> term <* symbol "of" <*> blocked (semiSep branch)

term2 :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
term2 = lambda <|> sig

patterns :: (MonadFail m, TokenParsing m) => m (Binder Text [Pattern Ann])
patterns = do pps <- sequenceA <$> some pattern1
              validate pps $ \n ->
                  unexpected $ "duplicate bindings in pattern for: " ++ unpack n
              return pps

lambda :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
lambda = lam <$> try (patterns <* reserve op "->") <*> term

term :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
term = letBlock <|> term2

letBlock :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Tm
letBlock = let_ <$ symbol "let" <*> blocked declarations <* symbol "in" <*> term

terms :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m [Tm]
terms = commaSep term

typeDecl :: (MonadFail m, TokenParsing m) => m TyDecl
typeDecl = (,) <$> try (termIdentifier <* reserve op ":") <*> annotation

termDeclClause :: forall m s. (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m)
               => m Text -> m (Text, PBody)
termDeclClause nameParser =
    join (try ((\tid pats gp -> gp <&> \g w -> (tid, pats g w))
              <$> nameParser <*> (PreBody <$> pattern0s)
              <*> guarded (reserve op "=")))
      <*> whereClause
 where
 pattern0s :: m (Binder Text [Pattern Ann])
 pattern0s = do ps <- sequenceA <$> many pattern0
                ps <$ validate ps
                        (\n -> unexpected $ "duplicate bindings in pattern for: " ++ unpack n)

guard :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m a -> m (Pair Tm)
guard side = Pair <$> term1 <* side <*> term

guarded :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m a -> m (m (Guarded Tm))
guarded side = (reserve op "|" *> pure (Guarded <$> sepBy1 (guard side) (reserve op "|")))
      <|> (side *> pure (Unguarded <$> term))

type PBody = PreBody Ann Text
type Where = Binder Text [Binding Ann Text]

whereClause :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m Where
whereClause = symbol "where" *> blocked declarations <|> pure (pure [])

declClause :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m (Either TyDecl (Text, PBody))
declClause = (Left <$> typeDecl) <|> (Right <$> termDeclClause termIdentifier)

declClauses :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m [Either TyDecl (Text, PBody)]
declClauses = semiSep declClause

type TyDecl = (Text, Ann)
type TmDecl = (Text, NonEmpty PBody)

decls :: (MonadState s m, MonadFail m) => m [Either TyDecl (Text, PBody)] -> m ([TyDecl], [TmDecl])
decls d = do (ts, cs) <- partitionEithers <$> d
             fmap (ts,) . mapM validateShape $ groupWith (^._1) cs
 where
 validateShape l = let (name:|_, pbs) = N.unzip l in
   if shapely pbs
     then return (name, pbs)
     else fail $ "Equations for `" ++ unpack name ++ "' have differing numbers of arguments."

validateDecls :: (MonadFail m) => [TyDecl] -> [TmDecl] -> m ()
validateDecls tys tms
  | Just n <- findDuplicate tyns =
    fail $ "Duplicate type declarations for `" ++ unpack n ++ "'."
  | Just n <- findDuplicate tmns =
    fail $ "Duplicate definitions for `" ++ unpack n ++ "'."
  | Just n <- uncovered tmns tyns =
    fail $ "No definition for declared value `" ++ unpack n ++ "'."
  | otherwise = return ()
 where
 tyns = map fst tys
 tmns = map fst tms

declarations' :: (MonadState s m, MonadFail m) => m [Either TyDecl (Text, PBody)] -> m (Binder Text [Binding Ann Text])
declarations' d = do
  (tys, tms) <- decls d
  let -- TODO: Rendering
      bindType s
        | Just t <- lookup s tys = explicit t
        | otherwise              = implicit
  bindings (extend (uncurry bindType) <$> tms) <$ validateDecls tys tms

declarations :: (MonadState s m, HasFixities s, MonadFail m, LayoutParsing m) => m (Binder Text [Binding Ann Text])
declarations = declarations' declClauses

uncovered :: Ord a => [a] -> [a] -> Maybe a
uncovered xs = find (`Set.notMember` s) where s = Set.fromList xs

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = flip evalState Set.empty . foldrM test Nothing
 where
 test e r = state $ \s -> if e `Set.member` s then (Just e, s) else (r, Set.insert e s)
