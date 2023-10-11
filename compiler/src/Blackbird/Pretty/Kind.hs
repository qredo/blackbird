{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Pretty.Kind
  (
  -- * Pretty Printing Kinds
    prettyHardKind
  , prettyKind
  , prettyKindF
  , prettySchema
  ) where

import Bound
import Blackbird.Pretty
import Blackbird.Syntax.Kind

-- $setup
-- >>> :set -XOverloadedStrings

prettyHardKind :: HardKind -> Doc s
prettyHardKind Unboxed = "#"
prettyHardKind Star = "*"
prettyHardKind Constraint = "Γ"
prettyHardKind Rho = "ρ"
prettyHardKind Phi = "φ"
prettyHardKind Native = "!"

-- | Pretty print a 'Kind'
prettyKind :: Kind String -> Bool -> Doc s
prettyKind (Var nm)     _ = pretty nm
prettyKind (HardKind h) _ = prettyHardKind h
prettyKind (Type k)     b = prettyKind k b
prettyKind (l :-> r)    b = parensIf b $ prettyKind l True <+> "->" <+> prettyKind r False

-- | Pretty print a 'Kind' with nontrivial variables
prettyKindF :: Applicative f => (a -> Int -> f (Doc s)) -> Kind a -> Int -> f (Doc s)
prettyKindF pa (Var nm) d = pa nm d
prettyKindF _ (HardKind h) _ = pure $ prettyHardKind h
prettyKindF pa (Type k) d = prettyKindF pa k d
prettyKindF pa (l :-> r) d = (\ld rd -> parensIf (d > 0) $ ld <+> "->" <+> rd) <$> prettyKindF pa l 1 <*> prettyKindF pa r 0

-- | Pretty print a 'Kind', using a fresh kind variable supply
--
-- You should have already removed any free variables from the variable set.
prettySchema :: Schema String -> [String] -> Doc s
prettySchema (Schema hl b) vs = prettyKind (instantiate (pure . (ns!!)) b) False
 where
 (ns, _) = chooseNames (const False) hl vs
