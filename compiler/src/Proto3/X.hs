--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2023 Qredo Ltd
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides extended support for the proto3 codec.
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

module Proto3.X
  ( Msg(..)
  , ProtoType(..)
  , (.*)
  , (:*)
  , (:+)
  , closedEnum
  , openEnum
  , end
  , map
  , oneof
  , repeated
  , singular
  , encode
  -- * Testing
  , roundtrip
  ) where

import Prelude hiding (map)

import Control.Applicative (liftA2)
import Control.Lens (Prism', lazy, preview, prism, prism', review, strict, view)
import Control.Monad (mfilter)
import Control.Monad.State ()
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (Foldable (fold, toList))
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import GHC.Exts (IsList (fromList))
import GHC.Generics qualified as G
import Generics.SOP (HCollapse (hcollapse))
import Generics.SOP qualified as SOP
import Proto3.Suite (FieldNumber, HasDefault (def), Primitive)
import Proto3.Suite qualified as Suite
import Proto3.Wire.Decode (ParseError, Parser (Parser, runParser), RawField, RawMessage, RawPrimitive)
import Proto3.Wire.Decode qualified as Decode
import Proto3.Wire.Encode qualified as Encode


type ProtoType :: Type -> Type -> Type
data ProtoType input a where
  Uint64 :: ProtoType RawPrimitive Word64 -- TODO: multiple encodings?
  String :: ProtoType RawPrimitive Text
  Bytes :: ProtoType RawPrimitive ByteString
  Enum :: EnumType a -> ProtoType RawPrimitive a
  Embedded :: Msg a b -> ProtoType RawPrimitive b -- TODO: make this optional? the protobuffer default value for messages is null

  Singular :: ProtoType RawPrimitive a -> ProtoType RawField a
  --  Optional :: ProtoType a -> ProtoType (Maybe a)
  Repeated :: ProtoType RawPrimitive a -> ProtoType RawField (Vector a) -- TODO: packed vs unpacked? list vs vector?

  -- TODO: keys need constraints?
  Map :: (Ord k, Suite.Primitive k)
      => ProtoType RawPrimitive k
      -> ProtoType RawPrimitive v
      -> ProtoType RawField (Map.Map k v)

type Schema :: Type -> Type -> Type
data Schema input a where
  End :: Schema RawMessage ()
  Field :: ProtoType RawField a -> Text -> FieldNumber -> Schema RawField a
  Product :: Schema RawField a -> Schema RawMessage b -> Schema RawMessage (a,b)
  OneOf :: Text -> Either a b -> Schema RawField a -> Schema RawField b -> Schema RawMessage (Either a b) --TODO: oneof fields cannot have rules directly

type EnumType :: Type -> Type
data EnumType a = EnumType
  Text
  a
  (Prism' Int32 a)
  (Map.Map Int32 [Text])

openEnum :: Text -> [Text] -> EnumType Int32
openEnum typename names = EnumType typename 0 (prism' id Just) cases
  where
    cases = Map.fromList $ zip [0..] $ fmap pure names

closedEnum :: Text -> [Text] -> EnumType Int32
closedEnum typename names = EnumType typename 0 (prism' id dec) cases
  where
    cases = Map.fromList $ zip [0..] $ fmap pure names
    minInvalid = fromIntegral (Map.size cases)
    dec = mfilter (< minInvalid) . Just

enumSchema :: EnumType a -> [Text]
enumSchema (EnumType name _ _ values) = fold
  [ ["enum " <> name <> " {"]
  , flip fmap definitions $ \(num, tag) -> tag <> " = " <> tshow num <> ";"
  , [ "}"]
  ]
  where
    definitions = Map.toList values >>= \(val, names) -> fmap (val,) names

-- TODO: Proto3.Wire.encode.enum does something strange?
enumEncode :: EnumType a -> FieldNumber -> a -> Encode.MessageBuilder
enumEncode (EnumType _ _ p _) n = Encode.int32 n . review p

enumDecode :: EnumType a -> Decode.Parser RawPrimitive a
enumDecode (EnumType _ _ p _) = do
  i <- Decode.int32
  Parser $ const $ case preview p i of
    Nothing -> Left $ Decode.BinaryError "Failed to decode enum value"
    Just e -> pure e

type Msg :: Type -> Type -> Type
data Msg pb a = Msg
  Text
  (Prism' pb a)
  (Schema RawMessage pb)

fieldNumber :: Schema RawField a -> FieldNumber
fieldNumber (Field _ _ n) = n

-- TODO: don't require msg name for encoding/decoding
unsugarMapType :: Text -> ProtoType RawPrimitive k -> ProtoType RawPrimitive v -> ProtoType RawField (Vector (k, v))
unsugarMapType msgName fk fv = Repeated $ Embedded $ Msg msgName (prism' enc dec) $
     singular fk "key" 1
  .* singular fv "value" 2
  .* end
  where
    enc (k,v) = (k, (v, ()))
    dec (k, (v, ())) = Just (k, v)

msgSource :: Msg pb a -> [Text]
msgSource (Msg msgName _ s) = ["message " <> msgName <> " {"] <> schemaSource s <> ["}"]
  where
    schemaSource :: Schema input x -> [Text]
    schemaSource = \case
      End -> mempty
      Field p name num -> [Text.unwords [fieldType p, name, "=", tshow num <> ";"]]
      Product a b -> schemaSource a <> schemaSource b
      OneOf name _defaultValue a b -> [ "oneof " <> name <> " { " ] <> schemaSource a <> schemaSource b <> ["}"] -- TODO: indentation

    fieldType :: ProtoType rule x -> Text
    fieldType = \case
      Uint64 -> "uint64"
      Bytes -> "bytes"
      String -> "string"
      Enum (EnumType name _ _ _) -> name
      Embedded (Msg name _ _) -> name
      Singular f -> fieldType f
      Repeated f -> "repeated " <> fieldType f
      Map k v -> fold ["map<", fieldType k, fieldType v, ">"]

encode :: Msg pb a -> a -> ByteString
encode m a = let (pb, f) = encode' m a in f pb

encode' :: Msg pb a -> a -> (pb, pb -> ByteString)
encode' m a = flip second (msgEncoder' m a) $ \f -> ByteString.toStrict . Encode.toLazyByteString . f

msgEncoder :: Msg pb a -> a -> Encode.MessageBuilder
msgEncoder m a = let (pb, f) = msgEncoder' m a in f pb

msgEncoder' :: Msg pb a -> a -> (pb, pb -> Encode.MessageBuilder)
msgEncoder' (Msg _ p s) a = (review p a,) $ \pb -> schemaEncoder s pb
  where
    schemaEncoder :: Schema input x -> x -> Encode.MessageBuilder
    schemaEncoder = \case
      End -> \() -> mempty
      Field ft _ num -> encField ft num
      Product x y -> \(x', y') -> schemaEncoder x x' <> schemaEncoder y y'
      OneOf _ _ x y -> either (schemaEncoder x) (schemaEncoder y)

    optField :: ProtoType input' x -> FieldNumber -> x -> Encode.MessageBuilder
    optField pt n x = if skipField pt x then mempty else encField pt n x

    skipField :: ProtoType input' x -> x -> Bool
    skipField = \case
      Uint64 -> (0 ==)
      Bytes -> ByteString.null
      String -> Text.null
      Enum (EnumType _ _ p' _) -> (0 ==) . review p'
      _ -> const False

    encField :: ProtoType input' x -> FieldNumber -> x -> Encode.MessageBuilder
    encField = \case
      Uint64 -> Encode.uint64
      Bytes -> Encode.byteString
      String -> \n -> Encode.text n . view lazy
      Enum e -> enumEncode e
      Singular ft -> optField ft
      Repeated ft -> Encode.vectorMessageBuilder . encField ft
      Embedded m -> \n -> Encode.embedded n . msgEncoder m
      Map fk fv -> \n -> encField (unsugarMapType "Don't care" fk fv) n . Vector.fromList . Map.toList

msgDecoder :: Msg pb a -> Decode.Parser RawMessage (Maybe a)
msgDecoder m = let (p, f) = msgDecoder' m in fmap f p

msgDecoder' :: Msg pb a -> (Decode.Parser RawMessage pb, pb -> Maybe a)
msgDecoder' (Msg _ pr schema) = (schemaDecoder schema, preview pr)
  where
    schemaDecoder :: Schema i x -> Decode.Parser RawMessage x
    schemaDecoder = \case
      End -> pure ()
      Field ft _ num -> goField ft `Decode.at` num
      Product a b -> liftA2 (,) (schemaDecoder a) (schemaDecoder b)
      OneOf _ defaultValue a b -> Decode.oneof defaultValue -- TODO: double-check spec
          [ (fieldNumber a, Left  <$> go a)
          , (fieldNumber b, Right <$> go b)
          ]
      where
        go :: Schema RawField x -> Decode.Parser RawField x
        go (Field ft _ _) = goField ft

    goPrimitive :: ProtoType RawPrimitive x -> Decode.Parser RawPrimitive x
    goPrimitive = \case
      Uint64 -> Decode.uint64
      Bytes -> Decode.byteString
      String -> view strict <$> Decode.text
      Enum e -> enumDecode e
      Embedded (Msg _ p s) -> prismParser $ Decode.embedded' $ schemaDecoder s
        where
          prismParser parser = Parser $ \input -> case runParser parser input of
            Left err -> Left err
            Right a -> case preview p a of
              Nothing -> Left $ Decode.BinaryError "Parsed protobuffer contents but failed when reverting prism" -- TODO: fail parsing or return Nothing?
              Just b -> pure b

    goField :: ProtoType RawField x -> Decode.Parser RawField x
    goField = \case
      Singular pt -> dec $ goPrimitive pt
        where
          dec = case pt of
           Embedded {} -> exactlyOne
           Bytes -> flip Decode.one def
           Uint64 -> flip Decode.one def
           String -> flip Decode.one def
           Enum (EnumType _ x _ _) -> flip Decode.one x
      Repeated pt -> fmap fromList $ Decode.repeated $ goPrimitive pt
      Map fk fv -> Map.fromList . toList <$> goField (unsugarMapType "Don't care" fk fv)

-- adapted from Proto3.Wire.Decode.one which has at-most-once semantics
exactlyOne :: Parser RawPrimitive a -> Parser RawField a
exactlyOne parser = Parser $ \xs -> case traverse (runParser parser) $ parsedField xs of
  Left err -> Left err
  Right Nothing -> Left $ Decode.EmbeddedError "Expected exactly one field" Nothing -- TODO: is this a good fit?
  Right (Just a) -> pure a
  where
    -- taken from Proto3.Wire.Decode as it's not currently exposed
    parsedField :: RawField -> Maybe RawPrimitive
    parsedField xs = case xs of
      [] -> Nothing
      (x:_) -> Just x

singular :: ProtoType RawPrimitive a -> Text -> FieldNumber -> Schema RawField a
singular = Field . Singular

repeated :: ProtoType RawPrimitive a -> Text -> FieldNumber -> Schema RawField (Vector a)
repeated = Field . Repeated

map :: (Ord k, Primitive k) => ProtoType RawPrimitive k -> ProtoType RawPrimitive v -> Text -> FieldNumber -> Schema RawField (Map.Map k v)
map k v = Field $ Map k v

infixr 5 .*
(.*) :: Schema RawField a -> Schema RawMessage b -> Schema RawMessage (a,b)
(.*) = Product

-- TODO: support more than 2 alternatives
oneof :: Text -> Either a b -> Schema RawField a -> Schema RawField b -> Schema RawMessage (Either a b)
oneof = OneOf

end :: Schema RawMessage ()
end = End

type Pair :: Type -> Type
data Pair a = Pair a a
  deriving stock (Eq, Show, G.Generic)
  deriving anyclass SOP.Generic

pairPB :: Msg (Int32 :* Word64 :* Word64 :* ()) (Pair Word64)
pairPB = Msg "wat" (prism' enc dec) $
    singular (Enum tag) "tag" 1
 .* singular Uint64 "a" 2
 .* singular Uint64 "b" 3
 .* End
  where
    enc (Pair a b) = (0, (a,(b,())))
    dec (t, (a,(b,()))) = case t == 0 of
      True -> Just $ Pair a b
      False -> Nothing

    tag = closedEnum "PairTag" $ hcollapse haskellTags

    haskellTags :: SOP.NP (SOP.K Text) (SOP.Code (Pair a))
    haskellTags = SOP.K "Pair" SOP.:* SOP.Nil

type Wat :: Type
data Wat
  = One Word64
  | Two (Pair Word64)
  deriving stock (Eq, Show, G.Generic)
  deriving anyclass SOP.Generic

watPB :: Msg (Word64 :+ Pair Word64) Wat
watPB = Msg "wat" (prism enc dec) $
  oneof "oneof" (Left 0)
    (singular Uint64 "a" 1)
    (singular (Embedded pairPB) "b" 2)
  where
    enc = \case
      One a -> Left a
      Two a -> Right a
    dec = Right . \case
      Left a -> One a
      Right a -> Two a

type Wut :: Type
data Wut
  = Tip Word64 Word64
  | Cons Word64 Wut
  deriving stock (Eq, Show, G.Generic)
  deriving anyclass SOP.Generic

wutPB :: Msg (Word64 :* (Word64 :+ Wut)) Wut
wutPB = Msg "wut" (prism enc dec) $
     singular Uint64 "a" 1
  .* oneof "lol" (Left 0)
        (singular Uint64 "b" 2)
        (singular (Embedded wutPB) "c" 3)
  where
    enc = \case
      Tip a b -> (a, Left b)
      Cons a w -> (a, Right w)
    dec = Right . \case
      (a, Left b) -> Tip a b
      (a, Right w) -> Cons a w

type Test :: Type
data Test
  = Test_A [Word64] Word64
  | Test_B String [Word64]
  deriving stock (Eq, Read, Show)

testPB :: Msg (Vector Word64 :* (Word64 :+ Text)) Test
testPB = Msg "TestPB" (prism enc dec) $
      repeated Uint64 "a" 1
  .* oneof "bc" (Left 0)
        (singular Uint64 "b" 2)
        (singular String "c" 3)
  where
    enc = \case
      Test_A ws w -> (fromList ws, Left w)
      Test_B s ws -> (fromList ws, Right $ Text.pack s)

    dec = Right . \case
      (ws, Left w) -> Test_A (toList ws) w
      (ws, Right s) -> Test_B (Text.unpack s) (toList ws)

type TestMap :: Type
data TestMap = TestMap (Map.Map Word64 Word64) --(Pair Word64))
  deriving stock (Eq, Show)

testMapPB :: Msg (Map.Map Word64 Word64 :* ()) TestMap
testMapPB = Msg "TestMapPB" (prism enc dec) $
     map Uint64 Uint64 "map" 1
  .* end
  where
    enc (TestMap m) = (m, ())
    dec (m, ()) = Right $ TestMap m

roundtrip
  :: Eq a
  => Msg pb a
  -> a
  -> (Bool, Either ParseError (Maybe a), Either ParseError pb, LazyByteString, pb)
roundtrip m a =
  let
    (b, enc) = msgEncoder' m a
    (dec, prv) = msgDecoder' m
    c = Encode.toLazyByteString $ enc b
    d = Decode.parse dec $ view strict c
    e = fmap prv d
  in
    (Right (Just a) == e, e, d, c, b)

tshow :: Show a => a -> Text
tshow = Text.pack . show

infixr 4 :*
type (:*) :: Type -> Type -> Type
type (:*) = (,)

infixr 4 :+
type (:+) :: Type -> Type -> Type
type (:+) = Either

-- >>> roundtrip pairPB $ Pair 0x41 0x42
-- (True,Right (Just (Pair 65 66)),Right (0,(65,(66,()))),"\DLEA\CANB",(0,(65,(66,()))))
-- >>> roundtrip watPB $ One 0x41
-- (True,Right (Just (One 65)),Right (Left 65),"\bA",Left 65)
-- >>> roundtrip watPB $ Two $ Pair 0x41 0x42
-- (True,Right (Just (Two (Pair 65 66))),Right (Right (Pair 65 66)),"\DC2\EOT\DLEA\CANB",Right (Pair 65 66))
-- >>> roundtrip wutPB $ Tip 0x41 0x42
-- (True,Right (Just (Tip 65 66)),Right (65,Left 66),"\bA\DLEB",(65,Left 66))
-- >>> roundtrip wutPB $ Cons 0x41 $ Tip 0x42 0x43
-- (True,Right (Just (Cons 65 (Tip 66 67))),Right (65,Right (Tip 66 67)),"\bA\SUB\EOT\bB\DLEC",(65,Right (Tip 66 67)))
-- >>> roundtrip wutPB $ Cons 0x41 $ Cons 0x42 $ Tip 0x43 0x44
-- (True,Right (Just (Cons 65 (Cons 66 (Tip 67 68)))),Right (65,Right (Cons 66 (Tip 67 68))),"\bA\SUB\b\bB\SUB\EOT\bC\DLED",(65,Right (Cons 66 (Tip 67 68))))
-- >>> roundtrip testPB $ Test_A [2,3,4] 1
-- (True,Right (Just (Test_A [2,3,4] 1)),Right ([2,3,4],Left 1),"\b\STX\b\ETX\b\EOT\DLE\SOH",([2,3,4],Left 1))
-- >>> roundtrip testPB $ Test_B "aa" [2,3,4]
-- (True,Right (Just (Test_B "aa" [2,3,4])),Right ([2,3,4],Right "aa"),"\b\STX\b\ETX\b\EOT\SUB\STXaa",([2,3,4],Right "aa"))
-- >>> roundtrip testMapPB $ TestMap mempty
-- (True,Right (Just (TestMap (fromList []))),Right (fromList [],()),"",(fromList [],()))
-- >>> roundtrip testMapPB $ TestMap $ Map.fromList [(1,2), (3,4), (5,6)]
-- (True,Right (Just (TestMap (fromList [(1,2),(3,4),(5,6)]))),Right (fromList [(1,2),(3,4),(5,6)],()),"\n\EOT\b\SOH\DLE\STX\n\EOT\b\ETX\DLE\EOT\n\EOT\b\ENQ\DLE\ACK",(fromList [(1,2),(3,4),(5,6)],()))
