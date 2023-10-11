--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2023 Qredo Ltd
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the native policy type.
--------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Blackbird.Native.Policy
  ( Policy(..)
  , Address(..)
  , _Policy_All
  , _Policy_Any
  , _Policy_Signature
  , _Address_Cooked
  , _Address_Raw
  ) where

import Control.Lens.TH (makePrisms)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

data Address
  = Address_Raw !Text !ByteString
  | Address_Cooked !Text !Text
  deriving stock (Generic, Eq, Ord, Read, Show)

-- | A policy expressed in native Haskell form.
data Policy
  = Policy_All [Policy]
  | Policy_Any !Word64 [Policy]
  | Policy_Signature !Address
  deriving stock (Generic, Eq, Read, Show)
  deriving anyclass (SOP.Generic)

makePrisms ''Policy
makePrisms ''Address
