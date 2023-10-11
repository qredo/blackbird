{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Syntax.Literal
  ( Literal(..)
  ) where

import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Data hiding (Fixity(..))
import Data.Hashable
import Data.Int
import Data.Text as SText
import GHC.Generics

-- | Primitive literal values used by patterns, terms and core.
data Literal
  = Int     !Int32
  | Long    !Int64
  | Byte    !Int8
  | Short   !Int16
  | String  SText.Text
  | Char    !Char
  | Float   !Float
  | Double  !Double
  | Integer !Integer
  | Person  SText.Text
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance Hashable Literal

instance Serial Literal where
  serialize (Int    i) = putWord8 0 >> serialize i
  serialize (Long   i) = putWord8 1 >> serialize i
  serialize (Byte   i) = putWord8 2 >> serialize i
  serialize (Short  i) = putWord8 3 >> serialize i
  serialize (String i) = putWord8 4 >> serialize i
  serialize (Char   i) = putWord8 5 >> serialize i
  serialize (Float  i) = putWord8 6 >> serialize i
  serialize (Double i) = putWord8 7 >> serialize i
  serialize (Integer i) = putWord8 8 >> serialize i
  serialize (Person  i) = putWord8 9 >> serialize i

  deserialize = getWord8 >>= \b -> case b of
    0 -> fmap Int    deserialize
    1 -> fmap Long   deserialize
    2 -> fmap Byte   deserialize
    3 -> fmap Short  deserialize
    4 -> fmap String deserialize
    5 -> fmap Char   deserialize
    6 -> fmap Float  deserialize
    7 -> fmap Double deserialize
    8 -> fmap Integer deserialize
    9 -> fmap Person deserialize
    _ -> fail $ "deserialize Literal: unexpected constructor tag: " ++ show b

instance Binary Literal where
  put = serialize
  get = deserialize

instance Serialize Literal where
  put = serialize
  get = deserialize
