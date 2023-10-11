--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Main ( main ) where

import Blackbird.Console.Options
import Blackbird.Console.Unicode
import Blackbird.Pretty
import Blackbird.Version
import Data.ByteString as B (putStr)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc)
import Proto3.X as PB (encode)
import System.Exit
import System.IO (stderr)
import Blackbird.Interchange.Policy (policyPB)
import Blackbird.Interpreter.Policy

main :: IO ()
main = withUnicode $ do
  optionParser <- parseOptions

  _opts <- execParser $ info (helper <*> optionParser) $
    fullDesc
    <> progDesc "The blackbird policy language"
    <> header ("blackbird " ++ version)

  s <- getContents
  r <- interpret s
  case r of
    Left doc -> do
      hSayLn stderr doc
      exitFailure
    Right x -> do
      B.putStr $ PB.encode policyPB x
      exitSuccess
