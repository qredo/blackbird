{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module supplies a number of builtin types intrinsic to the
-- Ermine language.
--
-- >>> infer int
-- *
--
-- >>> infer (list list)
-- *** Exception: kind mismatch: Type (HardKind Star) /= Type (HardKind Star) :-> Type (HardKind Star)
--------------------------------------------------------------------
module Blackbird.Builtin.Type
  (
  -- * Builtin Types
    Builtin(..)
  , builtin_
  -- ** Numerics
  , int, long, word, byte, short
  , float, double
  , inth, longh
  , integer
  , integerh
  -- ** Booleans
  , bool
  -- ** Text
  , string, char, stringh
  -- ** Containers
  , list
  , maybe_
  , ee
  , nlist
  , nlisth
  -- ** Policy
  , policy
  , policyh
  -- ** IO
  , io
  -- ** Type Equality
  , equality
  -- ** Classes
  , functor
  , lame
  , fromInteg
  -- ** Annotations
  , anyType
  ) where

import Bound
import Data.Kind (Type)
import Data.Text (pack)
import Blackbird.Syntax
import Blackbird.Syntax.Global
import Blackbird.Syntax.ModuleName
import Blackbird.Syntax.Type as Type
import Blackbird.Syntax.Kind as Kind

-- $setup
-- >>> :set -XRank2Types -XNoMonomorphismRestriction -XExtendedDefaultRules
-- >>> :m + Control.Lens Data.Void Ermine.Syntax.Type Ermine.Unification.Meta
-- >>> :m + Ermine.Unification.Kind Ermine.Inference.Kind
-- >>> :m + Text.Trifecta.Rendering Data.Functor.Identity Ermine.Pretty.Kind
-- >>> import Ermine.Pretty (names, plain, Doc, Pretty(..))
-- >>> let infer :: (forall k t. Type k t) -> Doc; infer t = prettySchema ?? names $ runM_ emptyRendering $ generalize =<< inferKind id t

------------------------------------------------------------------------------
-- Builtin
------------------------------------------------------------------------------

-- | This class allows for overloading of builtin types, so they can be used directly a nice DSL for specifying types
-- borrowing application from Haskell for the application of types.
class Builtin t where
  builtin :: Ord k => Kind k -> String -> t

instance Builtin HardType where
  builtin s n = con (builtin s n) (Kind.general s (const Nothing))

instance Builtin Global where
  builtin _ n = glob Idfix (mkModuleName_ "Builtin") (pack n)

instance Builtin (Typ k t) where
  builtin s n = HardType (builtin s n)

class Builtin' x where
  type K x :: Type
  type T x :: Type
  builtin' :: Ord k => Kind k -> String -> [Typ (K x) (T x)] -> x

instance (Builtin' y, x ~ Typ (K y) (T y)) => Builtin' (x -> y) where
  type K (x -> y) = K y
  type T (x -> y) = T y
  builtin' s n xs x = builtin' s n (x:xs)

instance Builtin' (Typ k a) where
  type K (Typ k a) = k
  type T (Typ k a) = a
  builtin' s n xs = apps (builtin s n) (reverse xs)

instance (Builtin' y, x ~ Typ (K y) (T y)) => Builtin (x -> y) where
  builtin s n y = builtin' s n [y]

-- | Create a 'Builtin' 'Typ' of 'Kind' 'star'.
builtin_ :: Builtin t => String -> t
builtin_  = builtin star

------------------------------------------------------------------------------
-- Numeric Types
------------------------------------------------------------------------------

-- |
-- >>> infer int
-- *
int :: Builtin t => t
int = builtin_ "Int"

-- |
-- >>> infer inth
-- #
inth :: Builtin t => t
inth = builtin unboxed "Int#"

-- |
-- >>> infer long
-- *
long :: Builtin t => t
long = builtin_ "Long"

-- |
-- >>> infer longh
-- #
longh :: Builtin t => t
longh = builtin unboxed "Long#"

-- |
-- >>> infer word
-- *
word :: Builtin t => t
word = builtin_ "Word"

-- |
-- >>> infer wordh
-- #
wordh :: Builtin t => t
wordh = builtin unboxed "Word#"

-- |
-- >>> infer byte
-- *
byte :: Builtin t => t
byte = builtin_ "Byte"

-- |
-- >>> infer short
-- *
short :: Builtin t => t
short = builtin_ "Short"

-- |
-- >>> infer float
-- *
float :: Builtin t => t
float = builtin_ "Float"

-- |
-- >>> infer integer
-- *
integer :: Builtin t => t
integer = builtin_ "Integer"

-- |
-- >>> infer integerh
-- !
integerh :: Builtin t => t
integerh = builtin native "Integer#"

-- |
-- >>> infer double
-- *
double :: Builtin t => t
double = builtin_ "Double"

-- |
-- >>> infer bool
-- *
bool :: Builtin t => t
bool = builtin_ "Bool"

------------------------------------------------------------------------------
-- Text
------------------------------------------------------------------------------

-- |
-- >>> infer char
-- *
char :: Builtin t => t
char = builtin_ "Char"

-- |
-- >>> infer string
-- *
string :: Builtin t => t
string = builtin_ "String"

stringh :: Builtin t => t
stringh = builtin native "String#"


------------------------------------------------------------------------------
-- Containers
------------------------------------------------------------------------------

-- |
-- >>> infer list
-- * -> *
--
-- >>> infer (list int)
-- *
--
-- >>> infer (tup [int, list int])
-- *

list :: Builtin t => t
list = builtin (star ~> star) "List"

-- |
-- >>> infer maybe_
-- * -> *
--
-- >>> infer (maybe_ int)
-- *
maybe_ :: Builtin t => t
maybe_ = builtin (star ~> star) "Maybe"

-- |
-- >>> infer ee
-- *
ee :: Builtin t => t
ee = builtin_ "E"

-- |
-- >>> infer equality
-- a -> a -> *
--
-- >>> infer (equality equality)
-- (a -> a -> *) -> *
equality :: Builtin t => t
equality = builtin ("a" ~> "a" ~> star) "Equality"

-- |
-- >>> infer nlisth
-- !
nlisth :: Builtin t => t
nlisth = builtin native "NList#"

-- |
-- >>> infer nlist
-- *
nlist :: Builtin t => t
nlist = builtin_ "NList"

-- |
-- >>> infer policy
-- *
policy :: Builtin t => t
policy = builtin_ "Policy"

-- |
-- >>> infer policyh
-- !
policyh :: Builtin t => t
policyh = builtin native "Policy#"

------------------------------------------------------------------------------
-- IO
------------------------------------------------------------------------------

io :: Builtin t => t
io = builtin (star ~> star) "IO"

------------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------------

-- |
-- >>> infer functor
-- (* -> *) -> Γ
--
-- >>> infer (equality functor)
-- ((* -> *) -> Γ) -> *
functor :: Builtin t => t
functor = builtin ((star ~> star) ~> constraint) "Functor"

-- | This is a placeholder typeclass we're using until we get class declaration
-- parsing implemented
--
-- >>> infer lame
-- * -> Γ
lame :: Builtin t => t
lame = builtin (star ~> constraint) "Lame"

-- >>> infer fromInteg
-- * -> Γ
fromInteg :: Builtin t => t
fromInteg = builtin (star ~> constraint) "FromInteger"

------------------------------------------------------------------------------
-- Annotations
------------------------------------------------------------------------------

-- | A type annotation that can be applied to anything.
anyType :: Annot k a
anyType = Annot [] [Nothing] $ Scope $ return $ B 0
