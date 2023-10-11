{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Pretty.Type
  (
  -- * Pretty Printing Kinds
    prettyHardType
  , prettyType
  , prettyTypeF
  , prettyTypeSchema
  ) where

import Bound
import Bound.Scope
import Control.Applicative (liftA2)
import Control.Arrow (left)
import Control.Comonad
import Control.Lens
import Data.Bifunctor (first)
import Data.Set (member, union, Set)
import Data.Set.Lens
import Data.Foldable
import Data.Text (unpack)
import Blackbird.Pretty
import Blackbird.Pretty.Kind
import Blackbird.Syntax.Global
import Blackbird.Syntax.Hint
import Blackbird.Syntax.Kind (kindVars, Kind)
import Blackbird.Syntax.Type

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

bananas :: Doc s -> Doc s
bananas xs = "(|" <> xs <> "|)"

prettyHardType :: HardType -> Doc s
prettyHardType (Tuple i) = parens (pretty (replicate (i-1) ','))
prettyHardType Arrow     = parens "->"
prettyHardType (Con (Global _ Idfix _ n) _)       = pretty (unpack n)
prettyHardType (Con (Global _ (Prefix _) _ n) _)  = parens ("prefix" <+> pretty (unpack n))
prettyHardType (Con (Global _ (Postfix _) _ n) _) = parens ("postfix" <+> pretty (unpack n))
prettyHardType (Con (Global _ _ _ n) _)           = parens (pretty (unpack n))
prettyHardType (ConcreteRho xs) = bananas (fillSep (punctuate "," (pretty . unpack <$> toList xs)))

skvs :: Ord k => [(Hint, Scope b Kind k)] -> Set k
skvs = setOf (traverse.traverse.traverseScope pure)

tkvs :: Ord k => Scope b (TK k) t -> Set k
tkvs (Scope e) = setOf (kindVars.traverse) e
         `union` setOf (traverse.traverse.kindVars.traverse) e

-- | Pretty print a 'Type' using a fresh variable supply and an ambient precedence.
--
-- You should have already removed any free variables' names from the supply.
prettyType :: Typ String String -> [String] -> Int -> Doc s
prettyType (HardType t) _  _ = prettyHardType t
prettyType (Var nm)     _  _ = pretty nm
prettyType (Loc _ r)    vs d = prettyType r vs d
prettyType (App (App (HardType Arrow) x) y) vs d =
  parensIf (d > 0) $ prettyType x vs 1 <+> "->" <+> prettyType y vs 0
prettyType (App f x)    vs d =
  parensIf (d > 10) $ prettyType f vs 10 <+> prettyType x vs 11
prettyType (And cs)     vs d = case cs of
  [c] -> prettyType c vs d
  _   -> tupled $ map (prettyType ?? vs ?? 0) cs
prettyType (Exists n ks cs) vs d =
  parensIf (d > 0) $ quantified (prettyType (inst cs) rvs 0)
 where
 kAvoid = member ?? skvs ks `union` tkvs cs
 tAvoid = member ?? setOf (traverseScope pure) cs
 (kvs, (tvs, rvs)) = chooseNames tAvoid (fmap fst ks) <$> chooseNames kAvoid n vs

 tks = map (\k -> prettyKind (instantiate (pure . (kvs!!)) $ extract k) False) ks

 inst = instantiateKindVars kvs . instantiate (pure . (tvs!!))

 consKinds | null kvs  = id
           | otherwise = (braces (fillSep (pretty <$> kvs)) :)

 quantified zs
   | null tvs  = zs
   | otherwise =
      hsep ("exists" :
              consKinds (zipWith (\tv tk -> parens (pretty tv <+> colon <+> tk))
                                 tvs tks)) <> dot
        <+> zs
prettyType (Forall n ks cs bdy) vs d =
  parensIf (d > 0) . quantified . constrained $ prettyType (inst bdy) rvs 0
 where
 kAvoid = member ?? skvs ks
            `union` tkvs cs
            `union` tkvs bdy
 tAvoid = member ?? setOf (traverseScope pure) cs `union` setOf (traverseScope pure) bdy

 (kvs, (tvs, rvs)) = chooseNames tAvoid (fmap fst ks) <$> chooseNames kAvoid n vs

 tks = map (\k -> prettyKind (instantiate (pure . (kvs!!)) $ extract k) False) ks

 inst = instantiateKindVars kvs . instantiate (pure . (tvs!!))

 consKinds | null kvs  = id
           | otherwise = (braces (fillSep (pretty <$> kvs)) :)

 quantified zs
   | null tvs && null kvs = zs
   | otherwise =
      hsep ("forall" :
              consKinds (zipWith (\tv tk -> parens (pretty tv <+> colon <+> tk))
                                 tvs tks)) <> dot
        <+> zs
 constrained zs
   | isTrivialConstraint . fromScope $ cs = zs
   | otherwise                            = prettyType (inst cs) rvs 0 <+> "=>" <+> zs

-- | Pretty print a 'Type' using a fresh variable supply and an ambient precedence.
--
-- You should have already removed any free variables' names from the supply.
prettyTypeF :: forall f k a s. Applicative f =>
               (k -> [String] -> Int -> f (Doc s)) ->
               (a -> [String] -> Int -> f (Doc s)) ->
               Typ k a -> [String] -> Int -> f (Doc s)
prettyTypeF _  _  (HardType t) _  _ = pure $ prettyHardType t
prettyTypeF _  pa (Var nm)     vs d = pa nm vs d
prettyTypeF pk pa (Loc _ r)    vs d = prettyTypeF pk pa r vs d
prettyTypeF pk pa (App (App (HardType Arrow) x) y) vs d =
  (\dx dy -> parensIf (d > 0) $ dx <+> "->" <+> dy) <$> prettyTypeF pk pa x vs 1 <*> prettyTypeF pk pa y vs 0
prettyTypeF pk pa (App f x)    vs d =
  parensIf (d > 10) <$> liftA2 (<+>) (prettyTypeF pk pa f vs 10) (prettyTypeF pk pa x vs 11)
prettyTypeF pk pa (And cs)     vs d = case cs of
  [c] -> prettyTypeF pk pa c vs d
  _   -> tupled <$> traverse (prettyTypeF pk pa ?? vs ?? 0) cs
prettyTypeF pk pa (Exists n ks cs) vs d =
  parensIf (d > 0) <$> quantified (prettyTypeF pesk pesa (inst cs) rvs 0)
 where
 -- TODO kAvoid and tAvoid really needed and if so how to do them?
 kAvoid = const False -- member ?? skvs ks `union` tkvs cs
 tAvoid = const False -- member ?? setOf (traverseScope pure) cs
 (kvs, (tvs, rvs)) = chooseNames tAvoid (fmap fst ks) <$> chooseNames kAvoid n vs

 tks :: f [Doc s]
 tks = traverse (\k -> prettyKindF (either (const . pure . pretty) (pk ?? vs)) (instantiateEither (pure . left (kvs!!)) $ extract k) d) ks

 inst :: Scope Int (TK k) a -> Typ (Either String k) (Either String a)
 inst = instantiateKindVars (map Left kvs) . first (fmap Right) . instantiateEither (pure . left (tvs!!))
 pesa :: Either String a -> [String] -> Int -> f (Doc s)
 pesa = either (const . const . pure . pretty) pa
 pesk :: Either String k -> [String] -> Int -> f (Doc s)
 pesk = either (const . const . pure . pretty) pk

 consKinds | null kvs  = id
           | otherwise = (braces (fillSep (pretty <$> kvs)) :)

 quantified zs
   | null tvs  = zs
   | otherwise =
      (\tkd zd -> hsep ("exists" :
              consKinds (zipWith (\tv tk -> parens (pretty tv <+> colon <+> tk))
                                 tvs tkd)) <> dot
        <+> zd) <$> tks <*> zs
prettyTypeF pk pa (Forall n ks cs bdy) vs d =
  parensIf (d > 0) <$> (quantified . constrained $ prettyTypeF pesk pesa (inst bdy) rvs 0)
 where
 -- TODO are these actually needed and if so how can they exist?
 -- kAvoid = member ?? skvs ks
 --            `union` tkvs cs
 --            `union` tkvs bdy
 -- tAvoid = member ?? setOf (traverseScope pure) cs `union` setOf (traverseScope pure) bdy
 kAvoid = const False
 tAvoid = const False

 (kvs, (tvs, rvs)) = chooseNames tAvoid (fmap fst ks) <$> chooseNames kAvoid n vs

 tks :: f [Doc s]
 tks = traverse (\k -> prettyKindF (either (const . pure . pretty) (pk ?? vs)) (instantiateEither (pure . left (kvs!!)) $ extract k) d) ks

 inst :: Scope Int (TK k) a -> Typ (Either String k) (Either String a)
 inst = instantiateKindVars (map Left kvs) . first (fmap Right) . instantiateEither (pure . left (tvs!!))
 pesa :: Either String a -> [String] -> Int -> f (Doc s)
 pesa = either (const . const . pure . pretty) pa
 pesk :: Either String k -> [String] -> Int -> f (Doc s)
 pesk = either (const . const . pure . pretty) pk

 consKinds | null kvs  = id
           | otherwise = (braces (fillSep (pretty <$> kvs)) :)

 quantified zs
   | null tvs && null kvs = zs
   | otherwise =
      (\tkd zd -> hsep ("forall" :
              consKinds (zipWith (\tv tk -> parens (pretty tv <+> colon <+> tk))
                                 tvs tkd)) <> dot
        <+> zd) <$> tks <*> zs
 constrained zs
   | isTrivialConstraint . fromScope $ cs = zs
   | otherwise                            = (\l r -> l <+> "=>" <+> r) <$> prettyTypeF pesk pesa (inst cs) rvs 0 <*> zs

prettyTypeSchema :: Scope Int (TK String) String -> ([Hint], [Hint])
                 -> [String] -> Doc s
prettyTypeSchema s (kn, tn) vs = prettyType (inst s) vs' 0
 where
 kAvoid = member ?? tkvs s
 tAvoid = member ?? setOf (traverseScope pure) s

 (kvs, (tvs, vs')) = chooseNames tAvoid tn <$> chooseNames kAvoid kn vs
 inst = instantiateKindVars kvs . instantiate (pure . (tvs!!))
