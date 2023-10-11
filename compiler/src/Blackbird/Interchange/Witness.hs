--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2023 Qredo Ltd
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides data interchange for the witness type
--------------------------------------------------------------------
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Blackbird.Interchange.Witness
  ( witnessPB
  ) where

import Blackbird.Native.Witness (Witness (..))
import Control.Lens (prism')
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Exts (IsList (fromList))
import Generics.SOP (HCollapse (hcollapse))
import Generics.SOP qualified as SOP
import Proto3.X as P3

witnessPB :: Msg (Int32 :* Vector Witness :* Map.Map Word64 Witness :* ()) Witness
witnessPB = Msg "Witness" (prism' enc dec) $
     singular (Enum tag) "tag" 1
  .* repeated (Embedded witnessPB) "all_witnesses" 2
  .* P3.map Uint64 (Embedded witnessPB) "any_witness" 3
  .* end
  where
    enc :: Witness -> (Int32, (Vector Witness, (Map.Map Word64 Witness, ())))
    enc = \case
      Witness_All ws              -> f 2 (fromList ws) mempty ()
      Witness_Any ws              -> f 3        mempty     ws ()
      Witness_PrecheckedSignature -> f 4        mempty mempty ()
      where f t a b c = (t,) $ (a,) $ (b,) c

    dec (t, (wsAll, (wsAny, ()))) = case (t, null wsAll, null wsAny) of
      (2, _, True) -> Just $ Witness_All $ toList wsAll
      (3, True, _) -> Just $ Witness_Any wsAny
      (4, True, True) -> Just Witness_PrecheckedSignature
      _ -> Nothing

    tag = closedEnum "WitnessTag" $
      [ "WITNESS_UNSPECIFIED"
      , "WITNESS_GUESS"
      ] <> hcollapse haskellTags

    haskellTags :: SOP.NP (SOP.K Text) (SOP.Code Witness)
    haskellTags =
             SOP.K "WITNESS_ALL"
      SOP.:* SOP.K "WITNESS_ANY"
      SOP.:* SOP.K "WITNESS_PRECHECKED_SIGNATURE"
      SOP.:* SOP.Nil

-- >>> roundtrip witnessPB $ Witness_All []
-- (True,Right (Just (Witness_All [])),Right (2,([],(fromList [],()))),"\b\STX",(2,([],(fromList [],()))))
-- >>> roundtrip witnessPB $ Witness_All [Witness_All [], Witness_All []]
-- (True,Right (Just (Witness_All [Witness_All [],Witness_All []])),Right (2,([Witness_All [],Witness_All []],(fromList [],()))),"\b\STX\DC2\STX\b\STX\DC2\STX\b\STX",(2,([Witness_All [],Witness_All []],(fromList [],()))))
-- >>> roundtrip witnessPB $ Witness_Any $ Map.fromList [(1, Witness_All [])]
-- (True,Right (Just (Witness_Any (fromList [(1,Witness_All [])]))),Right (3,([],(fromList [(1,Witness_All [])],()))),"\b\ETX\SUB\ACK\b\SOH\DC2\STX\b\STX",(3,([],(fromList [(1,Witness_All [])],()))))
