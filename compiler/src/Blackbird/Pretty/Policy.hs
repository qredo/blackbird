--------------------------------------------------------------------
-- |
-- Copyright :  (c) Qredo Ltd 2023
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Blackbird.Pretty.Policy
  ( prettyPolicy
  , prettyAddress
  ) where

import Control.Lens ((??))
import Data.Word (Word64)

import Blackbird.Native.Policy
import Blackbird.Pretty
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.Char as Char
import qualified Data.Text as Text

prettyAddress :: Address -> Doc s
prettyAddress = foldMap pretty . \case
  Address_Cooked prefix txt ->
    [ "@"
    , if Text.null prefix then mempty else prefixing prefix
    , if ambiguous txt then quoted txt else txt
    ]
  Address_Raw prefix bs ->
    [ "@"
    , prefixing prefix
    , "0x"
    , encodeBase16 bs
    ]
  where
    ambiguous txt = or
      [ "0x" `Text.isPrefixOf` txt
      , not $ Text.all (\c -> Char.isAlphaNum c || '.' == c) txt
      ]
    quoted x = "\"" <> escaped x <> "\""
    escaped x = Text.replace "\"" "\\\"" $ Text.replace "\\" "\\\\" x
    prefixing x = "<" <> x <> ">:"

prettyPolicy :: Policy -> Int -> Doc s
prettyPolicy (Policy_Signature s) _ = prettyAddress s
prettyPolicy (Policy_All ps) prec = prettyAll ps prec
prettyPolicy (Policy_Any k ps) prec
  | k <  1    = prettyAll [] prec
  | k == 1    = prettyOrs ps prec
  | otherwise = prettyAny k ps prec

prettyAll :: [Policy] -> Int -> Doc s
prettyAll [] prec = parensIf (prec > 10) "all []"
prettyAll [x] prec = prettyPolicy x prec
prettyAll (x:xs) prec = parensIf (prec > 3) $ prettyPolicy x 4 <+> "and" <+> prettyAll xs 3

prettyOrs :: [Policy] -> Int -> Doc s
prettyOrs [] prec = prettyAny 1 [] prec
prettyOrs [x] prec = prettyPolicy x prec
prettyOrs (x:xs) prec = parensIf (prec > 2) $ prettyPolicy x 3 <+> "or" <+> prettyOrs xs 2

prettyAny :: Word64 -> [Policy] -> Int -> Doc s
prettyAny k ps prec = parensIf (prec > 10) $
  "any" <+> pretty k <+> align (list (map (prettyPolicy ?? (-1)) ps))
