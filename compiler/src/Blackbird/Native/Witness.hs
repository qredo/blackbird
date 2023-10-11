--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2023 Qredo Ltd
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the native witness type.
--------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Blackbird.Native.Witness
  ( Witness(..)
  ) where

import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Generics.SOP qualified as SOP
import GHC.Generics

data Witness
  = Witness_All [Witness]
  | Witness_Any (Map.Map Word64 Witness)
  | Witness_PrecheckedSignature
  deriving (Eq, Show, Generic)

instance SOP.Generic Witness
