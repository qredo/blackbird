------------------------------------------------------------------------------
-- 
-- (C) 2023 Qredo Ltd.  Apache 2.0
--
------------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Generic.Data.Two (Eq2H(..), EqK(..), Generically2(..), LiftEq(..), (==>)) where

import Bound.Scope
import Data.Functor.Classes (Eq1(liftEq), Eq2(liftEq2))
import Data.Kind
import Data.Type.Equality ((:~:)(Refl))
import GHC.Generics
import GHC.TypeLits (Symbol)

infixl 5 ==>
(==>) :: forall {k1} {k2} {f :: k1 -> k2} {g :: k1 -> k2}.
         LiftEq f g
         -> forall (a :: k1) (b :: k1). LiftEqK a b -> LiftEqK (f a) (g b)
(==>) = unLiftEq

class EqK (f :: k) where
  liftEqK :: LiftEqK f f

type family LiftEqK (f :: k) (g :: k) = (r :: Type) | r -> k f g

data LiftEq f g = LiftEq { unLiftEq :: forall a b. LiftEqK a b -> LiftEqK (f a) (g b) }

type instance LiftEqK (f :: Type) (g :: Type) = f -> g -> Bool
type instance LiftEqK (f :: k -> k') (g :: k -> k') = LiftEq f g

class Eq2H f where
  liftEq2H :: forall a b c d. (LiftEq2HConstraints f a, LiftEq2HConstraints f b) => (forall x y. (x -> y -> Bool) -> a x -> b y -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool
  type LiftEq2HConstraints f :: (Type -> Type) -> Constraint

instance Eq b => Eq2H (Scope b) where
  liftEq2H f g x y = f (liftEq g) (fromScope x) (fromScope y)
  type LiftEq2HConstraints (Scope b) = Monad

newtype Generically2 f a b = Generically2 (f a b)

newtype A = A ()
newtype B = B ()

type Replace2 :: forall k. k -> Type -> Type -> k
type family Replace2 (t :: k) x y :: k where
  Replace2 (z :: Symbol) _ _ = z
  Replace2 (A :: Type) x _ = x
  Replace2 (B :: Type) _ y = y
  Replace2 (f a) x y = Replace2 f x y (Replace2 a x y)
  Replace2 z _ _ = z

class Replace2Helper s t x y where
  replace2proof :: Replace2 s x y :~: t

instance (Replace2Helper i i' x y, Replace2Helper c c' x y, Replace2Helper f f' x y, Replace2Helper u u' x y) => Replace2Helper (M1 i c f u) (M1 i' c' f' u') x y where
  replace2proof = case replace2proof @i @i' @x @y of
    Refl -> case replace2proof @c @c' @x @y of
      Refl -> case replace2proof @f @f' @x @y of
        Refl -> case replace2proof @u @u' @x @y of
          Refl -> Refl
instance (Replace2Helper i i' x y, Replace2Helper c c' x y, Replace2Helper u u' x y) => Replace2Helper (K1 i c u) (K1 i' c' u') x y where
  replace2proof = case replace2proof @i @i' @x @y of
    Refl -> case replace2proof @c @c' @x @y of
      Refl -> case replace2proof @u @u' @x @y of
        Refl -> Refl
instance (Replace2Helper f f' x y, Replace2Helper g g' x y, Replace2Helper u u' x y) => Replace2Helper ((f :+: g) u) ((f' :+: g') u') x y where
  replace2proof = case replace2proof @f @f' @x @y of
    Refl -> case replace2proof @g @g' @x @y of
      Refl -> case replace2proof @u @u' @x @y of
        Refl -> Refl
instance (Replace2Helper f f' x y, Replace2Helper g g' x y, Replace2Helper u u' x y) => Replace2Helper ((f :*: g) u) ((f' :*: g') u') x y where
  replace2proof = case replace2proof @f @f' @x @y of
    Refl -> case replace2proof @g @g' @x @y of
      Refl -> case replace2proof @u @u' @x @y of
        Refl -> Refl
instance Replace2Helper C C x y where replace2proof = Refl
instance Replace2Helper D D x y where replace2proof = Refl
instance Replace2Helper S S x y where replace2proof = Refl
instance Replace2Helper R R x y where replace2proof = Refl
instance (n ~ n', m ~ m', p ~ p', Replace2Helper nt nt' x y) => Replace2Helper ('MetaData n m p nt) ('MetaData n' m' p' nt') x y where
  replace2proof = case replace2proof @nt @nt' @x @y of
    Refl -> Refl
instance (n ~ n', Replace2Helper f f' x y, Replace2Helper r r' x y) => Replace2Helper ('MetaCons n f r) ('MetaCons n' f' r') x y where
  replace2proof = case replace2proof @f @f' @x @y of
    Refl -> case replace2proof @r @r' @x @y of
      Refl -> Refl
instance {-# OVERLAPPING #-} u ~ x => Replace2Helper A u x y where replace2proof = Refl
instance {-# OVERLAPPING #-} u ~ y => Replace2Helper B u x y where replace2proof = Refl
instance {-# OVERLAPPABLE #-} Replace2 u x y ~ u' => Replace2Helper u u' x y where replace2proof = Refl

replace2cast :: forall s x y t. Replace2Helper s t x y => t -> Replace2 s x y
replace2cast = case replace2proof @s @t @x @y of Refl -> id

class (Generic (f A B), Generic (f x y), Replace2Helper (Rep (f A B) ()) (Rep (f x y) ()) x y) => Rep2Helper f x y where
  from2 :: f x y -> Replace2 (Rep (f A B) ()) x y
  from2 v = replace2cast @(Rep (f A B) ()) @x @y @(Rep (f x y) ()) (from v)

instance (Generic (f A B), Generic (f x y), Replace2Helper (Rep (f A B) ()) (Rep (f x y) ()) x y) => Rep2Helper f x y

class Replace2 v x y ~ v => Eq2HelperHelper v x y
instance Replace2 v x y ~ v => Eq2HelperHelper v x y
 
class Eq2Helper t where
  liftEq2Helper :: forall a b c d. (a -> b -> Bool) -> (c -> d -> Bool) -> Replace2 t a c -> Replace2 t b d -> Bool

deriving newtype instance Eq2Helper (f x) => Eq2Helper (M1 i c f x)
instance {-# OVERLAPPING #-} Eq2Helper (K1 R A u) where
  liftEq2Helper f _ (K1 x) (K1 y) = f x y
instance {-# OVERLAPPING #-} Eq2Helper (K1 R B u) where
  liftEq2Helper _ g (K1 x) (K1 y) = g x y
instance {-# OVERLAPPABLE #-} (forall x y. Eq2HelperHelper c x y, Eq c) => Eq2Helper (K1 R c u) where
  liftEq2Helper _ _ (K1 x) (K1 y) = (x :: c) == (y :: c)
instance (Eq2Helper (f x), Eq2Helper (g x)) => Eq2Helper ((f :+: g) x) where
  liftEq2Helper f g (L1 x) (L1 y) = liftEq2Helper @(f x) f g x y
  liftEq2Helper f g (R1 x) (R1 y) = liftEq2Helper @(g x) f g x y
  liftEq2Helper _ _ _ _ = False
instance (Eq2Helper (f x), Eq2Helper (g x)) => Eq2Helper ((f :*: g) x) where
  liftEq2Helper f g (x :*: x') (y :*: y') = liftEq2Helper @(f x) f g x y && liftEq2Helper @(g x) f g x' y'

instance (forall x y. Rep2Helper f x y, Eq2Helper (Rep (f A B) ())) => Eq2 (Generically2 f) where
  liftEq2 :: forall a b c d. (a -> b -> Bool) -> (c -> d -> Bool) -> Generically2 f a c -> Generically2 f b d -> Bool
  liftEq2 f g (Generically2 x) (Generically2 y) = liftEq2Helper @(Rep (f A B) ()) @a @b @c @d f g (from2 @f @a @c x) (from2 @f @b @d y)

