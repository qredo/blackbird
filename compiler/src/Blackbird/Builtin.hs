{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Qredo Ltd 2023
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Blackbird.Builtin
  ( parserState
  , predefs
  , primOps
  , resolveGlobals
  ) where

import Bound
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.Int (Int32, Int64)
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Data.Word (Word64)
import Blackbird.Builtin.Core (cPutStrLn, cShowInt, cShowLong, cShowWord, cShowLongHash, cAddLong, cFromIntegerToInt, cFromIntegerToLong, cFromIntegerToWord, cFoldr, cNNil, cNCons, cAll, cAny, cAnd, cOr, cPerson)
import Blackbird.Builtin.Global (putStrLnhg, showInthg, showLonghg, addLonghg, literalg, fromIntegerToInthg, fromIntegerToLonghg, foldrg, nnilg, nnilhg, nconsg, nconshg, allhg, anyhg, andhg, orhg, personhg, fromIntegerToWordhg)
import Blackbird.Builtin.Head
import Blackbird.Builtin.Term as Term (dataCon)
import Blackbird.Builtin.Type as Type (lame, maybe_, ee, io, string, long, longh, int, word, integer, fromInteg, list, policy)
import Blackbird.Interpreter as Interp
import Blackbird.Native.Policy (Policy(..))
import Blackbird.Parser.State
import Blackbird.Syntax
import Blackbird.Syntax.Convention
import Blackbird.Syntax.Core as Core
import Blackbird.Syntax.Global as Global
import Blackbird.Syntax.Id
import Blackbird.Syntax.Kind as Kind
import Blackbird.Syntax.Literal (Literal(Long, Int))
import Blackbird.Syntax.Module
import Blackbird.Syntax.Type as Type
import Blackbird.Syntax.Term as Term

parserState :: ParseState
parserState = initialParserState & fixityDecls .~
  [ FixityDecl TermLevel (Infix R 5) ["::"]
  , FixityDecl TermLevel (Infix R 3) ["and"]
  , FixityDecl TermLevel (Infix R 2) ["or"]
  ]

clame :: Type.Typ k t
clame = con lame (star ~> constraint)
tyLame :: Type.Typ k t
tyLame = Forall [] [(Nothing,Scope star)]
           (Scope $ apps clame [pure $ B 0]) (Scope . pure $ B 0)
cfromInteger :: Type.Typ k t
cfromInteger = con fromInteg (star ~> constraint)
tyFromInteger :: Type.Typ k t
tyFromInteger = Forall [] [(Nothing,Scope star)]
                  (Scope $ apps cfromInteger [pure $ B 0])
                  (Scope $ integer ~> (pure $ B 0))
tyPSL :: Type.Typ k t
tyPSL = string ~> io (tuple 0)
tySI6 :: Type.Typ k t
tySI6 = long ~> string
tySI6H :: Type.Typ k t
tySI6H = longh ~> string
tySI :: Type.Typ k t
tySI = int ~> string
tySW :: Type.Typ k t
tySW = word ~> string
tyAL :: Type.Typ k t
tyAL = long ~> long ~> long
tyFI2L :: Type.Typ k t
tyFI2L = integer ~> long
tyFI2W :: Type.Typ k t
tyFI2W = integer ~> word
tyFoldr :: Type.Typ k t
tyFoldr = Forall
  []
  [(Nothing,Scope star),(Nothing,Scope star)]
  (Scope (And []))
  (Scope $
    ((pure $ B 0) ~> (pure $ B 1) ~> (pure $ B 1)) ~>
    (pure $ B 1) ~>
    apps list [pure $ B 0] ~>
    (pure $ B 1))
tyAll :: Type.Typ k t
tyAll = list policy ~> policy
tyAny :: Type.Typ k t
tyAny = word ~> list policy ~> policy
tyAnd :: Type.Typ k t
tyAnd = policy ~> policy ~> policy
tyOr :: Type.Typ k t
tyOr = policy ~> policy ~> policy
tyPerson :: Type.Typ k t
tyPerson = string ~> policy

predefs :: Text -> Term t Text
predefs "Nothing" =
  Term.dataCon Idfix "Builtin" "Nothing" $
    Forall [] [(Nothing, Scope star)]
           (Scope $ And []) (Scope . maybe_ . pure $ B 0)
predefs "Just"    =
  Term.dataCon Idfix "Builtin" "Just" $
    Forall [] [(Nothing, Scope star)]
           (Scope $ And []) (Scope $ pure (B 0) ~> maybe_ (pure $ B 0))
predefs "E"       =
  Term.dataCon Idfix "Builtin" "E" $
    Forall [] [(Nothing, Scope star)]
           (Scope $ And []) (Scope $ pure (B 0) ~> ee)
predefs "::"      =
  Term.dataCon (Infix R 5) "Builtin" "::" $
    Forall [] [(Nothing, Scope star)]
           (Scope $ And []) (Scope $ pure (B 0) ~> Type.list (pure $ B 0) ~> Type.list (pure $ B 0))
predefs "[]"      =
  Term.dataCon Idfix "Builtin" "[]" $
    Forall [] [(Nothing, Scope star)]
           (Scope $ And []) (Scope $ Type.list (pure $ B 0))
predefs  x = pure x

resolveGlobals :: AsConvention cc => Text -> Maybe (Type.Typ t k, Core cc c)
resolveGlobals txt
  | txt == "lame" = Just (tyLame, HardCore $ Slot 0)
  | txt == "fromInteger" = Just (tyFromInteger, HardCore $ Slot 0)
  | txt == "putStrLn" = Just (tyPSL, cPutStrLn)
  | txt == "showInt" = Just (tySI, cShowInt)
  | txt == "showWord" = Just (tySW, cShowWord)
  | txt == "showLong" = Just (tySI6, cShowLong)
  | txt == "showLongHash" = Just (tySI6H, cShowLongHash)
  | txt == "addLong" = Just (tyAL, cAddLong)
  | txt == "fromIntegerToLong" = Just (tyFI2L, cFromIntegerToLong)
  | txt == "fromIntegerToWord" = Just (tyFI2W, cFromIntegerToWord)
  | txt == "foldr" = Just (tyFoldr, cFoldr)
  | txt == "all" = Just (tyAll, cAll)
  | txt == "any" = Just (tyAny, cAny)
  | txt == "and" = Just (tyAnd, cAnd)
  | txt == "or" = Just (tyOr, cOr)
  | txt == "person" = Just (tyPerson, cPerson)
resolveGlobals _ = Nothing

primOps :: IO (HM.HashMap Id (Address IO))
primOps = do
  psl <- allocPrimOp $ primOpNZ Text.putStrLn
  si6 <- allocPrimOp . primOpUN $
           return . pack . show . (fromIntegral :: Word64 -> Int64)
  si <- allocPrimOp . primOpUN $
           return . pack . show . (fromIntegral :: Word64 -> Int32)
  al <- allocPrimOp . primOpUUU $ (+)
  dli <- allocGlobal (error "primOps: dli") $
           Dict [] [Scope $ Data [U] 0 literalg [_Lit # Int 5]]
  dll <- allocGlobal (error "primOps: dll") $
           Dict [] [Scope $ Data [U] 0 literalg [_Lit # Long 37]]
  dfii <- allocGlobal (error "primOps: dfii") $
           Dict [] [Scope $ cFromIntegerToInt ]
  dfil <- allocGlobal (error "primOps: dfil") $
           Dict [] [Scope $ cFromIntegerToLong ]
  fi2i <- allocPrimOp . primOpNU $
    return . fromIntegral . (fromInteger :: Integer -> Int32)
  fi2l <- allocPrimOp . primOpNU $
    return . fromIntegral . (fromInteger :: Integer -> Int64)
  fi2w <- allocPrimOp . primOpNU $
    return . (fromInteger :: Integer -> Word64)
  foldr_ <- allocGlobal (error "primOps: foldr") cFoldr
  nnil <- allocGlobal (error "primOps: nnil") cNNil
  ncons <- allocGlobal (error "primOps: ncons") cNCons
  nnilh <- allocPrimOp . primOpN $
    return []
  nconsh <- allocPrimOp . primOpNNN $
    (return .) . (:)
  allh <- allocPrimOp . primOpNN $
    return . Policy_All
  anyh <- allocPrimOp . primOpUNN $
    (return .) . Policy_Any
  andh <- allocPrimOp . primOpNNN $
    (return .) . (\x -> \case
      Policy_All xs -> Policy_All (x:xs)
      y -> Policy_All [x,y]
    )
  orh <- allocPrimOp . primOpNNN $
    (return .) . (\x -> \case
      Policy_Any 1 xs -> Policy_Any 1 (x:xs)
      y -> Policy_Any 1 [x,y]
    )
  personh <- allocPrimOp . primOpNN $
    return . Policy_Signature
  pure $ HM.fromList
          [ (_Global # putStrLnhg, psl)
          , (_Global # showInthg, si)
          , (_Global # showLonghg, si6)
          , (_Global # addLonghg, al)
          , (_Global # fromIntegerToInthg, fi2i)
          , (_Global # fromIntegerToLonghg, fi2l)
          , (_Global # fromIntegerToWordhg, fi2w)
          , (_Global # foldrg, foldr_)
          , (_Global # nnilg, nnil)
          , (_Global # nnilhg, nnilh)
          , (_Global # nconsg, ncons)
          , (_Global # nconshg, nconsh)
          , (_Global # allhg, allh)
          , (_Global # anyhg, anyh)
          , (_Global # andhg, andh)
          , (_Global # orhg, orh)
          , (_Global # personhg, personh)
          , (InstanceId hlameL, dll)
          , (InstanceId hlameI, dli)
          , (InstanceId hfromIntegerI, dfii)
          , (InstanceId hfromIntegerL, dfil)
          ]
