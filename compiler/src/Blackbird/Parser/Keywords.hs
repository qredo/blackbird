--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Parser.Keywords
  ( keywords
  , otherKeywords
  , startingKeywords
  ) where

import Data.HashSet

-- | This is the set of keywords that can only occur at the beginning of the line for auto-completion purposes.
startingKeywords :: HashSet String
startingKeywords = fromList
  [ "abstract"
  , "class"
  , "data"
  , "database"
  , "export"
  , "field"
  , "foreign"
  , "import"
  , "instance"
  , "private"
  , "type"
  ]

-- | This is the set of keywords that can occur anywhere on the line for auto-completion purposes.
otherKeywords :: HashSet String
otherKeywords = fromList
  [ "and"
  , "case"
  , "constraint"
  , "constructor"
  , "do"
  , "exists"
  , "forall"
  , "hole"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "let"
  , "of"
  , "or"
  , "phi"
  , "postfix"
  , "prefix"
  , "rho"
  , "subtype"
  , "table"
  , "where"
  , "_"
  , "Γ"
  , "ρ"
  , "φ"
  ]

-- | The set of all keywords.
--
-- @'keywords' = 'startingKeywords' '<>' 'otherKeywords'@
keywords :: HashSet String
keywords = startingKeywords <> otherKeywords
