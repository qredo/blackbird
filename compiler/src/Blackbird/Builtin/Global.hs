{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable (DeriveDataTypeable)
--
--------------------------------------------------------------------
module Blackbird.Builtin.Global
  (
  -- * lists
    nilg
  , consg
  -- * maybe
  , nothingg
  , justg
  -- * tuples
  , tupleg
  -- * booleans
  , trueg
  , falseg
  -- * policies
  , allg
  , allhg
  , anyg
  , anyhg
  , andg
  , andhg
  , org
  , orhg
  , persong
  , personhg
  -- * existential test constructor
  , eg
  -- * numeric literals
  , literalg
  -- * string literals
  , stringg
  -- * primitive types
  , stringhg
  , inthg
  , wordhg
  , longhg
  , integerhg
  , nlisthg
  , policyhg
  -- * primops
  , putStrLng
  , putStrLnhg
  , showIntg
  , showInthg
  , showLongg
  , showLonghg
  , showWordg
  , showWordhg
  , addLongg
  , addLonghg
  , fromIntegerToIntg
  , fromIntegerToInthg
  , fromIntegerToWordg
  , fromIntegerToWordhg
  , fromIntegerToLongg
  , fromIntegerToLonghg
  , foldrg
  , nnilg
  , nnilhg
  , nconsg
  , nconshg
  ) where

import Data.Text
import Data.Word
import Blackbird.Syntax.Global
import Blackbird.Syntax.ModuleName

builtin :: Fixity -> Text -> Global
builtin f = glob f (mkModuleName_ "Builtin")

builtin_ :: Text -> Global
builtin_ = builtin Idfix

nilg, consg, nothingg, justg, eg :: Global
nilg     = builtin_ "[]"
consg    = builtin (Infix R 5) "::"
nothingg = builtin (Infix R 5) "Nothing"
justg    = builtin_ "Just"
eg       = builtin_ "E"

tupleg :: Word64 -> Global
tupleg w8 = builtin_ $ pack $ "(" ++ Prelude.replicate (if n == 0 then 0 else n-1) ',' ++ ")"
  where n :: Int; n = fromIntegral w8

trueg, falseg :: Global
trueg  = builtin_ "True"
falseg = builtin_ "False"

allg, allhg, anyg, anyhg, andg, andhg, org, orhg, persong, personhg :: Global
allg = builtin_ "all"
allhg = builtin_ "all#"
anyg = builtin_ "any"
anyhg = builtin_ "any#"
andg = builtin (Infix R 3) "and"
andhg = builtin_ "and#"
org = builtin (Infix R 2) "or"
orhg = builtin_ "or#"
persong = builtin_ "person"
personhg = builtin_ "person#"

literalg :: Global
literalg = builtin_ "Literal"

stringg :: Global
stringg = builtin_ "String"

stringhg :: Global
stringhg = builtin_ "String#"

inthg :: Global
inthg = builtin_ "Int#"

wordhg :: Global
wordhg = builtin_ "Word#"

longhg :: Global
longhg = builtin_ "Long#"

integerhg :: Global
integerhg = builtin_ "Integer#"

nlisthg :: Global
nlisthg = builtin_ "NList#"

policyhg :: Global
policyhg = builtin_ "Policy#"

putStrLng :: Global
putStrLng = builtin_ "putStrLn"

putStrLnhg :: Global
putStrLnhg = builtin_ "putStrLn#"

showIntg :: Global
showIntg = builtin_ "showInt"

showInthg :: Global
showInthg = builtin_ "showInt#"

showLongg :: Global
showLongg = builtin_ "showLong"

showLonghg :: Global
showLonghg = builtin_ "showLong#"

showWordg :: Global
showWordg = builtin_ "showWord"

showWordhg :: Global
showWordhg = builtin_ "showWord#"

addLongg :: Global
addLongg = builtin_ "addLong"

addLonghg :: Global
addLonghg = builtin_ "addLong#"

fromIntegerToIntg :: Global
fromIntegerToIntg = builtin_ "fromIntegerToInt"

fromIntegerToInthg :: Global
fromIntegerToInthg = builtin_ "fromIntegerToInt#"

fromIntegerToLongg :: Global
fromIntegerToLongg = builtin_ "fromIntegerToLong"

fromIntegerToLonghg :: Global
fromIntegerToLonghg = builtin_ "fromIntegerToLong#"

fromIntegerToWordg :: Global
fromIntegerToWordg = builtin_ "fromIntegerToWord"

fromIntegerToWordhg :: Global
fromIntegerToWordhg = builtin_ "fromIntegerToWord#"

foldrg :: Global
foldrg = builtin_ "foldr"

nnilg :: Global
nnilg = builtin_ "nnil"

nnilhg :: Global
nnilhg = builtin_ "nnil#"

nconsg :: Global
nconsg = builtin_ "ncons"

nconshg :: Global
nconshg = builtin_ "ncons#"
