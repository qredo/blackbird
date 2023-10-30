{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Demo.Server (serve) where

import Blackbird.Interchange.Policy (policyPB)
import Blackbird.Interchange.Witness (witnessPB)
import Blackbird.Interpreter.Policy (interpret, residuate, satisfiable, satisfies, signatures, stallers)
import Blackbird.Native.Policy (Policy, Address (..))
import Blackbird.Pretty.Policy (prettyPolicy, prettyAddress)
import Blackbird.Pretty.Witness (prettyWitness)
import Control.Applicative (asum)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Base64.URL (encodeBase64Unpadded)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Snap.Core (Snap, ifTop, readRequestBody, route, setResponseStatus, writeBS, writeLBS, writeText)
import Snap.Http.Server (quickHttpServe)
import Snap.Internal.Core (modifyResponse)
import Snap.Util.CORS (applyCORS, defaultOptions)
import Text.Read (readMaybe)

import Proto3.X qualified as Protobuffers

serve :: IO ()
serve = quickHttpServe site

index :: ByteString
index = $(makeRelativeToProject "src/Demo/index.html" >>= embedFile)

site :: Snap ()
site = applyCORS defaultOptions $ asum
  [ ifTop (writeBS index)
  , route
      [ ("compile", compile)
      , ("verify", verifyHandler)
      ]
  ]

jsonRoute :: Aeson.FromJSON a => (a -> Snap ()) -> Snap ()
jsonRoute f = (Aeson.eitherDecode <$> readRequestBody 16384) >>= \case
  Left msg -> err $ "Could not decode request body: " <> Text.pack msg
  Right body -> f body

err :: Text -> Snap ()
err msg = do
  modifyResponse $ setResponseStatus 400 "Bad Request"
  writeText msg

withParsedPolicy :: Text -> (Policy -> Snap ()) -> Snap ()
withParsedPolicy src f = do
  liftIO (interpret $ Text.unpack src) >>= \case
    Left doc -> err $ tshow doc
    Right p -> f p

encodePB :: (ByteString -> Text) -> Protobuffers.Msg pb a -> a -> Aeson.Value
encodePB f pb = Aeson.String . f . Protobuffers.encode pb

compile :: Snap ()
compile = jsonRoute $ \(policySource :: Text) -> do
  withParsedPolicy policySource $ \case
    p
      | not $ satisfiable p -> err "This policy cannot be satisfied."
      | satisfies (const False) p -> err "This policy requires no approval."
      | otherwise -> writeLBS $ Aeson.encode $ Map.fromList @Text @Aeson.Value
        [ ("parse", Aeson.String $ tshow p)
        , ("protobuffer_base16", encodePB (("0x" <>) . encodeBase16) policyPB p)
        , ("protobuffer_base64url", encodePB encodeBase64Unpadded policyPB p)
        , ("signatures", encodeSignatures $ signatures p)
        , ("stallers", encodeSignatures $ fromMaybe Set.empty $ stallers p)
        ]
      where
        encodeSignatures :: Set Address -> Aeson.Value
        encodeSignatures = Aeson.Array . fmap encodeSignature . Vector.fromList . Set.toList

        encodeSignature :: Address -> Aeson.Value
        encodeSignature a = Aeson.Array $ Vector.fromList [str a, str (prettyAddress a)]
        where
          str = Aeson.String . tshow

verifyHandler :: Snap ()
verifyHandler = jsonRoute $ \(policyADT :: Text, sigADTs :: Set Text) ->
  let
    with parse x name f = case parse x of
      Nothing -> err $ "Invalid " <> name <> ": " <> x
      Just y -> f y

    validSigs :: Set Address
    validSigs = Set.fromList $ mapMaybe tread $ Set.toList sigADTs

    invalidSigs :: [Text]
    invalidSigs = flip mapMaybe (Set.toList sigADTs) $ \s -> maybe (Just s) (const Nothing) $ tread @Address s

    ppWitness (w, s) = Map.fromList @Text @Aeson.Value
      [ ("protobuffer_base16", encodePB (("0x" <>) . encodeBase16) witnessPB w)
      , ("protobuffer_base64url", encodePB encodeBase64Unpadded witnessPB w)
      , ("signers", Aeson.Array $ Vector.fromList $ fmap (Aeson.String . tshow . prettyAddress) $ Set.toList s)
      , ("prettyprint", Aeson.String $ Text.pack $ show $ prettyWitness w)
      ]

    pp = bimap ppWitness $ show . flip prettyPolicy (-1)
  in
    with tread policyADT "policy" $ \p ->
      case null invalidSigs of
        False -> err $ "Found invalid signatures: " <> Text.intercalate ", " invalidSigs
        True -> writeLBS $ Aeson.encode $ pp $ residuate (`Set.member` validSigs) p

tshow :: Show a => a -> Text
tshow = Text.pack . show

tread :: Read a => Text -> Maybe a
tread = readMaybe . Text.unpack
