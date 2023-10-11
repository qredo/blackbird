--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2023 Qredo Ltd
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides data interchange for the policy type.
--------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Blackbird.Interchange.Policy
  ( policyPB
  ) where

import Blackbird.Native.Policy (Address (..), Policy (..))
import Control.Arrow ((>>>))
import Control.Lens (prism')
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (toList))
import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Exts (IsList (fromList))
import Generics.SOP (HCollapse (hcollapse))
import Generics.SOP qualified as SOP
import Proto3.X as P3

policyPB :: Msg (Int32 :* Word64 :* Vector Policy :* Text :* (Text :+ ByteString)) Policy
policyPB = Msg "Policy" (prism' enc dec) $
     singular (Enum tag) "tag" 1
  .* singular Uint64 "threshold" 2 -- TODO: support 'optional' properly
  .* repeated (Embedded policyPB) "subpolicies" 3
  .* singular String "address_prefix" 16
  .* oneof "address" (Left "")
    (singular String "cooked_address" 4)
    (singular Bytes "raw_address" 5)
  where
    enc = \case
      Policy_All ps        -> f 1 0 (fromList ps) "" (Left "")
      Policy_Any k ps      -> f 2 k (fromList ps) "" (Left "")
      Policy_Signature sig -> f 3 0 mempty `uncurry` case sig of
        Address_Cooked prefix s -> (prefix, Left s)
        Address_Raw prefix s -> (prefix, Right s)
      where f t a b c d = (t, (a, (b, (c, d))))

    dec = f >>> \case
      (1, 0, ps, "", Left "") -> Just $ Policy_All $ toList ps
      (2, k, ps, "", Left "") -> Just $ Policy_Any k $ toList ps
      (3, 0, ps, prefix, sig) -> case null ps of
        False -> Nothing
        True -> Just $ Policy_Signature $ either (flip Address_Cooked) (flip Address_Raw) sig prefix
      _ -> Nothing -- TODO: errors
      where f (t, (a, (b, (c, d)))) = (t, a, b, c, d)

    tag = closedEnum "PolicyTag" $ "POLICY_UNSPECIFIED" : hcollapse haskellTags

    haskellTags :: SOP.NP (SOP.K Text) (SOP.Code Policy)
    haskellTags =
             SOP.K "POLICY_ALL"
      SOP.:* SOP.K "POLICY_ANY"
      SOP.:* SOP.K "POLICY_SIGNATURE"
      SOP.:* SOP.Nil

-- >>> raw = Policy_Signature $ Address_Raw "xx" "aa"
-- >>> cooked = Policy_Signature $ Address_Cooked "yy" "bb"
-- >>> roundtrip policyPB raw
-- (True,Right (Just (Policy_Signature (Address_Raw "xx" "aa"))),Right (3,(0,([],("xx",Right "aa")))),"\b\ETX\130\SOH\STXxx*\STXaa",(3,(0,([],("xx",Right "aa")))))
-- >>> roundtrip policyPB $ Policy_All [raw, Policy_All []]
-- (True,Right (Just (Policy_All [Policy_Signature (Address_Raw "xx" "aa"),Policy_All []])),Right (1,(0,([Policy_Signature (Address_Raw "xx" "aa"),Policy_All []],("",Left "")))),"\b\SOH\SUB\v\b\ETX\130\SOH\STXxx*\STXaa\SUB\STX\b\SOH",(1,(0,([Policy_Signature (Address_Raw "xx" "aa"),Policy_All []],("",Left "")))))
-- >>> roundtrip policyPB $ Policy_Any 2 [Policy_Any 1 [raw, cooked], cooked, Policy_All []]
-- (True,Right (Just (Policy_Any 2 [Policy_Any 1 [Policy_Signature (Address_Raw "xx" "aa"),Policy_Signature (Address_Cooked "yy" "bb")],Policy_Signature (Address_Cooked "yy" "bb"),Policy_All []])),Right (2,(2,([Policy_Any 1 [Policy_Signature (Address_Raw "xx" "aa"),Policy_Signature (Address_Cooked "yy" "bb")],Policy_Signature (Address_Cooked "yy" "bb"),Policy_All []],("",Left "")))),"\b\STX\DLE\STX\SUB\RS\b\STX\DLE\SOH\SUB\v\b\ETX\130\SOH\STXxx*\STXaa\SUB\v\b\ETX\130\SOH\STXyy\"\STXbb\SUB\v\b\ETX\130\SOH\STXyy\"\STXbb\SUB\STX\b\SOH",(2,(2,([Policy_Any 1 [Policy_Signature (Address_Raw "xx" "aa"),Policy_Signature (Address_Cooked "yy" "bb")],Policy_Signature (Address_Cooked "yy" "bb"),Policy_All []],("",Left "")))))
