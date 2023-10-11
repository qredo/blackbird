--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Pretty.Literal
  ( prettyLiteral
  ) where

import Blackbird.Pretty
import Blackbird.Syntax.Literal

-- | Pretty print any 'Literal'.
prettyLiteral :: Literal -> Doc s
prettyLiteral (Int i)    = pretty $ show i
prettyLiteral (Long l)   = pretty $ show l ++ "L"
prettyLiteral (Byte b)   = pretty $ show b ++ "B"
prettyLiteral (Short s)  = pretty $ show s ++ "S"
prettyLiteral (String s) = pretty $ show s
prettyLiteral (Integer i)= pretty $ show i ++ "I"
prettyLiteral (Char c)   = pretty $ show c
prettyLiteral (Float f)  = pretty $ show f ++ "F"
prettyLiteral (Double d) = pretty $ show d
prettyLiteral (Person p) = pretty "@" <> pretty p
