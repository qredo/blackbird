-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2011-2019, Qredo Ltd 2023
-- License     :  BSD3
--
-- Maintainer  :  Qredo LTD <support@qredo.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
{-# language BangPatterns           #-}
{-# language CPP                    #-}
{-# language DeriveFoldable         #-}
{-# language DeriveFunctor          #-}
{-# language DeriveTraversable      #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses  #-}
{-# language Rank2Types             #-}
{-# language TemplateHaskell        #-}
module Blackbird.Parser.Trifecta
  ( Parser(..)
  , LayoutParsing(layouted)
  , manyAccum
  -- * Feeding a parser more input
  , Step(..)
  , feed
  , starve
  , stepParser
  , stepResult
  , stepIt
  -- * Parsing
  , runParser
  , parseFromFile
  , parseFromFileEx
  , parseString
  , parseByteString
  , parseTest
  -- * Parsers
  , betwixt
  , angles
  , braces
  , brackets
  , parens
  ) where

import Control.Applicative as Alternative
import Control.Monad (MonadPlus(..), ap, join)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.State.Lazy as Lazy (StateT(..))
import Control.Monad.State.Strict as Strict (StateT(..))
import Data.ByteString as Strict hiding (empty, snoc)
import Data.ByteString.UTF8 as UTF8
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isJust)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Reducer
-- import Data.Sequence as Seq hiding (empty)
import Data.Set as Set hiding (empty, toList)
-- import Debug.Trace (trace)
import Prettyprinter as Pretty (Doc, LayoutOptions(LayoutOptions), PageWidth(AvailablePerLine), SimpleDocStream, layoutPageWidth, layoutSmart, line')
import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.IO
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Parser.Token (TokenParsing(..), symbolic)
import Text.Trifecta.Combinators
import Text.Trifecta.Delta       as Delta
import Text.Trifecta.Rendering
import Text.Trifecta.Result
import Text.Trifecta.Rope
import Text.Trifecta.Util.It

data LayoutState
  = Layout_None
  | Layout_Indent ByteString
  | Layout_Nest ByteString
  | Layout_OneLine ByteString
  deriving (Show)

data Freezing
  = Freeze_None
  | Freeze_Outdent
  | Freeze_Unnest
  deriving (Show)

data Marker = Marker !Freezing !Int Delta

-- | The type of a trifecta parser
--
-- The first four arguments are behavior continuations:
--
--   * epsilon success: the parser has consumed no input and has a result
--     as well as a possible Err; the position and chunk are unchanged
--     (see `pure`)
--
--   * epsilon failure: the parser has consumed no input and is failing
--     with the given Err; the position and chunk are unchanged (see
--     `empty`)
--
--   * committed success: the parser has consumed input and is yielding
--     the result, set of expected strings that would have permitted this
--     parse to continue, new position, and residual chunk to the
--     continuation.
--
--   * committed failure: the parser has consumed input and is failing with
--     a given ErrInfo (user-facing error message)
--
-- The remaining arguments are
--
--   * the layout state
--
--   * an indication of whether the parser is allowed to consume input
--
--   * indent if at the beginning of a line
--
--   * the current position
--
--   * the chunk of input currently under analysis
--
-- `Parser` is an `Alternative`; trifecta's backtracking behavior encoded as
-- `<|>` is to behave as the leftmost parser which yields a value
-- (regardless of any input being consumed) or which consumes input and
-- fails.  That is, a choice of parsers will only yield an epsilon failure
-- if *all* parsers in the choice do.  If that is not the desired behavior,
-- see `try`, which turns a committed parser failure into an epsilon failure
-- (at the cost of error information).
newtype Parser a = Parser
  { unparser :: forall r.
       (a -> Err -> It Rope r)
    -> (Err -> It Rope r)
    -> (a -> Set String -> Freezing -> Int -> Delta -> ByteString -> It Rope r)  -- committed success
    -> (ErrInfo -> It Rope r)                                 -- committed err
    -> LayoutState
    -> Freezing
    -> Int                                                    -- indent
    -> Delta
    -> ByteString
    -> It Rope r
  }

tp :: String -> (a -> b -> c -> d -> LayoutState -> Freezing -> Int -> Delta -> ByteString -> e) -> a -> b -> c -> d -> LayoutState -> Freezing -> Int -> Delta -> ByteString -> e
tp = flip const
-- tp s p = \a b c d l f i dd bs -> trace ("called " <> s <> " " <> show (l,f,i,dd,bs)) p a b c d l f i dd bs

instance Functor Parser where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# inlinable fmap #-}
  a <$ Parser m = Parser $ \ eo ee co -> m (\_ -> eo a) ee (\_ -> co a)
  {-# inlinable (<$) #-}

instance Applicative Parser where
  pure a = Parser $ \ eo _ _ _ _ _ _ _ _ -> eo a mempty
  {-# inlinable pure #-}
  (<*>) = ap
  {-# inlinable (<*>) #-}

instance Alternative Parser where
  empty = Parser $ \_ ee _ _ _ _ _ _ _ -> ee mempty
  {-# inlinable empty #-}
  Parser m <|> Parser n = Parser $ \ eo ee co ce l f i d bs ->
    m eo (\e -> n (\a e' -> eo a (e <> e')) (\e' -> ee (e <> e')) co ce l f i d bs) co ce l f i d bs
  {-# inlinable (<|>) #-}
  many p = Prelude.reverse <$> manyAccum (:) p
  {-# inlinable many #-}
  some p = (:) <$> p <*> Alternative.many p

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)
  {-# inlinable (<>) #-}

instance Monoid a => Monoid (Parser a) where
  mappend = (<>)
  {-# inlinable mappend #-}

  mempty = pure mempty
  {-# inlinable mempty #-}

instance Monad Parser where
  return = pure
  {-# inlinable return #-}
  Parser m >>= k = Parser $ \ eo ee co ce l f i d bs ->
    m -- epsilon result: feed result to monadic continutaion; committed
      -- continuations as they were given to us; epsilon callbacks merge
      -- error information with `<>`
      (\a e -> unparser (k a) (\b e' -> eo b (e <> e')) (\e' -> ee (e <> e')) co ce l f i d bs)
      -- epsilon error: as given
      ee
      -- committed result: feed result to monadic continuation and...
      (\a es f' i' d' bs' -> unparser (k a)
         -- epsilon results are now committed results due to m consuming.
         --
         -- epsilon success is now committed success at the new position
         -- (after m), yielding the result from (k a) and merging the
         -- expected sets (i.e. things that could have resulted in a longer
         -- parse)
         (\b e' -> co b (es <> _expected e') f' i' d' bs')
         -- epsilon failure is now a committed failure at the new position
         -- (after m); compute the error to display to the user
         (\e ->
           let errDoc = explain (renderingCaret d' bs') e { _expected = _expected e <> es }
               errDelta = _finalDeltas e
           in  ce $ ErrInfo errDoc (d' : errDelta)
         )
         -- committed behaviors as given; nothing exciting here
         co ce
         -- new position and remaining chunk after m
         l f' i' d' bs')
      -- committed error, delta, and bytestring: as given
      ce l f i d bs
  {-# inlinable (>>=) #-}
  (>>) = (*>)
  {-# inlinable (>>) #-}
#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
  {-# inlinable fail #-}
#endif

instance Fail.MonadFail Parser where
  fail s = Parser $ \ _ ee _ _ _ _ _ _ _ -> ee (failed s)
  {-# inlinable fail #-}

instance MonadPlus Parser where
  mzero = empty
  {-# inlinable mzero #-}
  mplus = (<|>)
  {-# inlinable mplus #-}

manyAccum :: (a -> [a] -> [a]) -> Parser a -> Parser [a]
manyAccum f (Parser p) = Parser $ \eo _ co ce l fr i d bs ->
  let walk xs x es f' i' d' bs' = p (manyErr d' bs') (\e -> co (f x xs) (_expected e <> es) f' i' d' bs') (walk (f x xs)) ce l f' i' d' bs'
      manyErr d' bs' _ e  = ce (ErrInfo errDoc [d'])
        where errDoc = explain (renderingCaret d' bs') (e <> failed "'many' applied to a parser that accepted an empty string")
  in p (manyErr d bs) (eo []) (walk []) ce l fr i d bs

liftIt :: It Rope a -> Parser a
liftIt m = Parser $ \ eo _ _ _ _ _ _ _ _ -> do
  a <- m
  eo a mempty
{-# inlinable liftIt #-}

instance Parsing Parser where
  try (Parser m) = Parser $ \ eo ee co _ -> m eo ee co (\_ -> ee mempty)
  {-# inlinable try #-}
  Parser m <?> nm = Parser $ \ eo ee -> m
     (\a e -> eo a (if isJust (_reason e) then e { _expected = Set.singleton nm } else e))
     (\e -> ee e { _expected = Set.singleton nm })
  {-# inlinable (<?>) #-}
  skipMany p = () <$ manyAccum (\_ _ -> []) p
  {-# inlinable skipMany #-}
  unexpected s = Parser $ \ _ ee _ _ _ _ _ _ _ -> ee $ failed $ "unexpected " ++ s
  {-# inlinable unexpected #-}
  eof = notFollowedBy anyChar <?> "end of input"
  {-# inlinable eof #-}
  notFollowedBy p = lookAhead $ try (optional p >>= maybe (pure ()) (unexpected . show))
  {-# inlinable notFollowedBy #-}

instance Errable Parser where
  raiseErr e = Parser $ \ _ ee _ _ _ _ _ _ _ -> ee e
  {-# inlinable raiseErr #-}

-- XXX is looking through freezes the behavior we want in general?
instance LookAheadParsing Parser where
  lookAhead (Parser m) = Parser $ \eo ee _ ce l _ -> m eo ee (\a _ _ _ _ _ -> eo a mempty) ce l Freeze_None
  {-# inlinable lookAhead #-}

instance CharParsing Parser where
  satisfy f = Parser $ tp "satisfy" $ \ _ ee co _ _ fr i d bs ->
    case fr of
      Freeze_None -> case UTF8.uncons $ Strict.drop (fromIntegral (columnByte d)) bs of
        Nothing        -> ee (failed "unexpected EOF")
        Just (c, xs)
          | not (f c)       -> ee mempty
          | Strict.null xs  -> let !ddc = d <> delta c
                                   i' :: Int
                                   !i' = if i >= 0 && isSpace c then fromIntegral (columnByte ddc) else -1
                                   i'' :: Int
                                   !i'' = if c == '\n' then 0 else i'
                               in join $ fillIt (co c mempty Freeze_None i'' ddc (if c == '\n' then mempty else bs))
                                                (co c mempty Freeze_None i'')
                                                ddc
          | otherwise       -> let !ddc = d <> delta c
                                   i' :: Int
                                   !i' = if i >= 0 && isSpace c then fromIntegral (columnByte ddc) else -1
                               in co c mempty Freeze_None i' ddc bs
      _ -> ee mempty -- FIXME should probably not be mempty?
  {-# inlinable satisfy #-}

checkSpace :: Parser ()
checkSpace = Parser $ \eo ee co _ l _ i d bs ->
  let spaceError = ee mempty in
  case l of
    Layout_None -> eo () mempty
    Layout_OneLine dent
      | i >= 0 && i <= Strict.length dent && Strict.isPrefixOf (Strict.take i bs) dent -> co () mempty Freeze_Outdent i d bs
      | i >= 0 && i > Strict.length dent && Strict.isPrefixOf dent bs -> co () mempty Freeze_Unnest i d bs
      | i >= 0 -> spaceError
      | otherwise -> eo () mempty
    Layout_Indent dent
      | i >= 0 && i <= Strict.length dent && Strict.isPrefixOf (Strict.take i bs) dent -> co () mempty Freeze_Outdent i d bs
      | i >= 0 && i > Strict.length dent && Strict.isPrefixOf dent bs -> eo () mempty
      | i >= 0 -> spaceError
      | otherwise -> eo () mempty
    Layout_Nest dent
      | i >= 0 && i <= Strict.length dent && Strict.isPrefixOf (Strict.take i bs) dent -> co () mempty Freeze_Outdent i d bs
      | i >= 0 && i > Strict.length dent && Strict.isPrefixOf dent bs -> eo () mempty
      | i >= 0 -> spaceError
      | otherwise -> eo () mempty

indentOf :: LayoutState -> Strict.ByteString
indentOf Layout_None = mempty
indentOf (Layout_Indent x) = x
indentOf (Layout_Nest x) = x
indentOf (Layout_OneLine x) = x

class TokenParsing p => LayoutParsing p where
  layouted :: p a -> p a

instance LayoutParsing Parser where
  layouted (Parser p) = Parser $ \_ ee co ce l f i d bs ->
    let
      l' = case f of
        Freeze_None | i >= 0 -> Layout_Indent $ Strict.take i bs
        _ -> Layout_OneLine $ indentOf l
      eo' a e = co' a (_expected e) f i d bs
      co' = case f of
        Freeze_None | i >= 0 -> co_Indent
        _ -> co_OneLine
      refreeze i' = case l of
        Layout_None -> Freeze_None
        Layout_OneLine dent
          | i' >= 0 && i' <= Strict.length dent -> Freeze_Outdent
          | i' >= 0 && i' > Strict.length dent -> Freeze_Unnest
          | otherwise -> Freeze_None
        Layout_Indent dent
          | i' >= 0 && i' <= Strict.length dent -> Freeze_Outdent
          | i' >= 0 && i' > Strict.length dent -> Freeze_None
          | otherwise -> Freeze_None
        Layout_Nest dent
          | i' >= 0 && i' <= Strict.length dent -> Freeze_Outdent
          | i' >= 0 && i' > Strict.length dent -> Freeze_None
          | otherwise -> Freeze_None
      co_Indent a e f' i' d' bs' = case f' of
        Freeze_None
          | i' < 0 -> co a e Freeze_Unnest i' d' bs'
          | i' >= i -> co a e Freeze_Unnest i' d' bs'
          | otherwise -> co a e (refreeze i') i' d' bs'
        Freeze_Outdent
          | i' >= i -> co a e Freeze_Unnest i' d' bs' -- > case can't happen, but == case can!
          | otherwise -> co a e (refreeze i') i' d' bs'
          --  | i' < 0 -> CAN'T HAPPEN
        Freeze_Unnest -> co a e Freeze_Unnest i' d' bs'
      co_OneLine a e f' i' d' bs' = case f' of
        Freeze_None -> co a e Freeze_Unnest i' d' bs'
        Freeze_Outdent -> co a e (refreeze i') i' d' bs'
        Freeze_Unnest -> co a e Freeze_Unnest i' d' bs'
    in p eo' ee co' ce l' f i d bs

instance (LayoutParsing m, MonadPlus m) => LayoutParsing (Lazy.StateT s m) where
  layouted (Lazy.StateT p) = Lazy.StateT $ \s -> layouted (p s)

instance (LayoutParsing m, MonadPlus m) => LayoutParsing (Strict.StateT s m) where
  layouted (Strict.StateT p) = Strict.StateT $ \s -> layouted (p s)

invisibleSemi :: Parser Char
invisibleSemi = Parser $ tp "invisibleSemi" $ \_ ee co _ l f i d bs ->
  case l of
    Layout_None -> ee mempty
    Layout_Indent dent -> case f of
      Freeze_None -> ee mempty
      Freeze_Outdent
        | i == Strict.length dent -> co ';' mempty Freeze_None (-1) d bs
        | otherwise -> ee mempty
      Freeze_Unnest -> ee mempty
    Layout_Nest _ -> ee mempty
    Layout_OneLine _ -> ee mempty

instance TokenParsing Parser where
  someSpace = skipMany (satisfy isSpace) *> checkSpace
  nesting (Parser p) = Parser $ \eo ee co ce l f i d bs ->
    let
      refreeze i' = case l of
        Layout_None -> Freeze_None
        Layout_OneLine dent
          | i' >= 0 && i' <= Strict.length dent -> Freeze_Outdent
          | i' >= 0 && i' > Strict.length dent -> Freeze_Unnest
          | otherwise -> Freeze_None
        Layout_Indent dent
          | i' >= 0 && i' <= Strict.length dent -> Freeze_Outdent
          | i' >= 0 && i' > Strict.length dent -> Freeze_None
          | otherwise -> Freeze_None
        Layout_Nest dent
          | i' >= 0 && i' <= Strict.length dent -> Freeze_Outdent
          | i' >= 0 && i' > Strict.length dent -> Freeze_None
          | otherwise -> Freeze_None
      co' a e f' i' d' bs' = case f' of
        Freeze_None -> co a e Freeze_None i' d' bs'
        Freeze_Outdent -> co a e Freeze_Outdent i' d' bs'
        Freeze_Unnest -> co a e (refreeze i') i' d' bs'
      l' = case l of
        Layout_Indent dent -> Layout_Nest dent
        other -> other
    in p eo ee co' ce l' f i d bs
  semi = invisibleSemi <|> token (satisfy (';'==) <?> ";")

instance DeltaParsing Parser where
  line = Parser $ \eo _ _ _ _ _ _ _ bs -> eo bs mempty
  {-# inlinable line #-}
  position = Parser $ \eo _ _ _ _ _ _ d _ -> eo d mempty
  {-# inlinable position #-}
  rend = Parser $ \eo _ _ _ _ _ _ d bs -> eo (rendered d bs) mempty
  {-# inlinable rend #-}
  slicedWith f p = do
    m <- position
    a <- p
    r <- position
    f a <$> liftIt (sliceIt m r)
  {-# inlinable slicedWith #-}

instance HasDelta Marker where
  delta (Marker _ _ d) = d

instance MarkParsing Marker Parser where
  mark = Parser $ \eo _ _ _ _ f i d _ -> eo (Marker f i d) mempty
  {-# inlinable mark #-}
  release (Marker f' i' d') = Parser $ \_ ee co _ _ _ _ d bs -> do
    mbs <- rewindIt d'
    case mbs of
      Just bs' -> co () mempty f' i' d' bs'
      Nothing
        | bytes d' == bytes (rewind d) + fromIntegral (Strict.length bs) -> if near d d'
            then co () mempty f' i' d' bs
            else co () mempty f' i' d' mempty
        | otherwise -> ee mempty

-- | A 'Step' allows for incremental parsing, since the parser
--
--   - can be done with a final result
--   - have errored
--   - can have yielded a partial result with possibly more to come
data Step a
  = StepDone !Rope a
    -- ^ Parsing is done and has converted the 'Rope' to a final result

  | StepFail !Rope ErrInfo
    -- ^ Parsing the 'Rope' has failed with an error

  | StepCont !Rope (Result a) (Rope -> Step a)
    -- ^ The 'Rope' has been partially consumed and already yielded a 'Result',
    -- and if more input is provided, more results can be produced.
    --
    -- One common scenario for this is to parse log files: after parsing a
    -- single line, that data can already be worked with, but there may be more
    -- lines to come.

instance Show a => Show (Step a) where
  showsPrec d (StepDone r a) = showParen (d > 10) $
    showString "StepDone " . showsPrec 11 r . showChar ' ' . showsPrec 11 a
  showsPrec d (StepFail r xs) = showParen (d > 10) $
    showString "StepFail " . showsPrec 11 r . showChar ' ' . showsPrec 11 xs
  showsPrec d (StepCont r fin _) = showParen (d > 10) $
    showString "StepCont " . showsPrec 11 r . showChar ' ' . showsPrec 11 fin . showString " ..."

instance Functor Step where
  fmap f (StepDone r a)    = StepDone r (f a)
  fmap _ (StepFail r xs)   = StepFail r xs
  fmap f (StepCont r z k)  = StepCont r (fmap f z) (fmap f . k)

-- | Feed some additional input to a 'Step' to continue parsing a bit further.
feed :: Reducer t Rope => t -> Step r -> Step r
feed t (StepDone r a)    = StepDone (snoc r t) a
feed t (StepFail r xs)   = StepFail (snoc r t) xs
feed t (StepCont r _ k)  = k (snoc r t)
{-# inlinable feed #-}

-- | Assume all possible input has been given to the parser, execute it to yield
-- a final result.
starve :: Step a -> Result a
starve (StepDone _ a)    = Success a
starve (StepFail _ xs)   = Failure xs
starve (StepCont _ z _)  = z
{-# inlinable starve #-}

stepResult :: Rope -> Result a -> Step a
stepResult r (Success a)  = StepDone r a
stepResult r (Failure xs) = StepFail r xs
{-# inlinable stepResult #-}

stepIt :: It Rope a -> Step a
stepIt = go mempty where
  go r m = case simplifyIt m r of
    Pure a -> StepDone r a
    It a k -> StepCont r (pure a) $ \r' -> go r' (k r')
{-# inlinable stepIt #-}

data Stepping a
  = EO a Err
  | EE Err
  | CO a (Set String) Delta ByteString
  | CE ErrInfo

-- | Incremental parsing. A 'Step' can be supplied with new input using 'feed',
-- the final 'Result' is obtained using 'starve'.
stepParser
    :: Parser a
    -> Delta -- ^ Starting cursor position. Usually 'mempty' for the beginning of the file.
    -> Step a
stepParser (Parser p) d0 = joinStep $ stepIt $ do
  bs0 <- fromMaybe mempty <$> rewindIt d0
  go bs0 <$> p eo ee co ce Layout_None Freeze_None 0 d0 bs0
 where
  eo a e            = Pure (EO a e)
  ee e              = Pure (EE e)
  co a es _ _ d' bs = Pure (CO a es d' bs)
  ce errInf         = Pure (CE errInf)

  go :: ByteString -> Stepping a -> Result a
  go _   (EO a _)     = Success a
  go bs0 (EE e)       = Failure $
                          let errDoc = explain (renderingCaret d0 bs0) e
                          in  ErrInfo errDoc (d0 : _finalDeltas e)
  go _   (CO a _ _ _) = Success a
  go _   (CE e)       = Failure e

  joinStep :: Step (Result a) -> Step a
  joinStep (StepDone r (Success a)) = StepDone r a
  joinStep (StepDone r (Failure e)) = StepFail r e
  joinStep (StepFail r e)           = StepFail r e
  joinStep (StepCont r a k)         = StepCont r (join a) (joinStep <$> k)
  {-# inlinable joinStep #-}

-- | Run a 'Parser' on input that can be reduced to a 'Rope', e.g. 'String', or
-- 'ByteString'. See also the monomorphic versions 'parseString' and
-- 'parseByteString'.
runParser
    :: Reducer t Rope
    => Parser a
    -> Delta -- ^ Starting cursor position. Usually 'mempty' for the beginning of the file.
    -> t
    -> Result a
runParser p d bs = starve $ feed bs $ stepParser p d
{-# inlinable runParser #-}

-- | @('parseFromFile' p filePath)@ runs a parser @p@ on the input read from
-- @filePath@ using 'ByteString.readFile'. All diagnostic messages emitted over
-- the course of the parse attempt are shown to the user on the console.
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Nothing -> return ()
-- >     Just a  -> print $ sum a
parseFromFile :: MonadIO m => Parser a -> String -> m (Maybe a)
parseFromFile p fn = do
  result <- parseFromFileEx p fn
  case result of
   Success a  -> return (Just a)
   Failure xs -> do
     liftIO $ renderIO stdout $ renderPretty 0.8 80 $ (_errDoc xs) <> line'
     return Nothing

-- | @('parseFromFileEx' p filePath)@ runs a parser @p@ on the input read from
-- @filePath@ using 'ByteString.readFile'. Returns all diagnostic messages
-- emitted over the course of the parse and the answer if the parse was
-- successful.
--
-- > main = do
-- >   result <- parseFromFileEx (many number) "digits.txt"
-- >   case result of
-- >     Failure xs -> displayLn xs
-- >     Success a  -> print (sum a)
parseFromFileEx :: MonadIO m => Parser a -> String -> m (Result a)
parseFromFileEx p fn = do
  s <- liftIO $ Strict.readFile fn
  return $ parseByteString p (Directed (UTF8.fromString fn) 0 0 0 0) s

-- | Fully parse a 'UTF8.ByteString' to a 'Result'.
--
-- @parseByteString p delta i@ runs a parser @p@ on @i@.
parseByteString
    :: Parser a
    -> Delta -- ^ Starting cursor position. Usually 'mempty' for the beginning of the file.
    -> UTF8.ByteString
    -> Result a
parseByteString = runParser

-- | Fully parse a 'String' to a 'Result'.
--
-- @parseByteString p delta i@ runs a parser @p@ on @i@.
parseString
    :: Parser a
    -> Delta -- ^ Starting cursor position. Usually 'mempty' for the beginning of the file.
    -> String
    -> Result a
parseString = runParser

parseTest :: (MonadIO m, Show a) => Parser a -> String -> m ()
parseTest p s = case parseByteString p mempty (UTF8.fromString s) of
  Failure xs -> liftIO $ renderIO stdout $ renderPretty 0.8 80 $ (_errDoc xs) <> line' -- TODO: retrieve columns
  Success a  -> liftIO (print a)

renderPretty :: Double -> Int -> Doc AnsiStyle -> SimpleDocStream AnsiStyle
renderPretty ribbonFraction page
  = layoutSmart LayoutOptions { layoutPageWidth = AvailablePerLine page ribbonFraction }

betwixt :: TokenParsing m => Char -> Char -> m a -> m a
betwixt a b = between (symbolic a) (symbolic b) . nesting

braces :: TokenParsing m => m a -> m a
braces = betwixt '{' '}'

brackets :: TokenParsing m => m a -> m a
brackets = betwixt '[' ']'

angles :: TokenParsing m => m a -> m a
angles = betwixt '<' '>'

parens :: TokenParsing m => m a -> m a
parens = betwixt '(' ')'
