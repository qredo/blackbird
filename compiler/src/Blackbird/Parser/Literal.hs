--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for 'literal' terms or patterns
--------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Blackbird.Parser.Literal
  ( literal
  ) where

import Blackbird.Parser.Style
import Blackbird.Syntax.Literal
import Control.Applicative
import qualified Data.Text as Text
import Text.Parser.Combinators (unexpected)
import Text.Parser.Token (TokenParsing, charLiteral, naturalOrDouble, stringLiteral)

-- | Parse a 'literal' number, string or character.
literal :: forall m. (Monad m, TokenParsing m) => m Literal
literal = either Integer Double <$> naturalOrDouble
      <|> String <$> stringLiteral
      <|> Char <$> charLiteral
      <|> (person . Text.drop 1 =<< personIdentifier)
  where
    person :: Text.Text -> m Literal
    person t
      | Text.null t = unexpected "nameless participant"
      | otherwise = pure $ Person t
