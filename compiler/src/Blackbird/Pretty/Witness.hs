--------------------------------------------------------------------
-- |
-- Copyright :  (c) Qredo Ltd 2023
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Blackbird.Pretty.Witness
  ( prettyWitness
  ) where

import Data.Map qualified as Map
import Data.Tree (drawTree, Tree (Node))
import Data.Word (Word64)

import Blackbird.Native.Witness (Witness(..))
import Blackbird.Pretty

prettyWitness :: Witness -> Doc s
prettyWitness = pretty . drawTree . prettyWitnessTree ""

prettyWitnessTree :: String -> Witness -> Tree String
prettyWitnessTree prefix = \case
  Witness_PrecheckedSignature -> n "Signature" []
  Witness_All ws -> n "all" $ fmap (prettyWitnessTree "") ws
  Witness_Any ws -> n "any" $ prettyAny $ Map.toList ws
  where
    n txt = Node $ case prefix of
      "" -> txt
      _ -> prefix <> ":" <> txt

prettyAny :: [(Word64,Witness)] -> [Tree String]
prettyAny = fmap $ \(i,x) -> prettyWitnessTree (show i) x
