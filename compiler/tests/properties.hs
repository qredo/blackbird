{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Framework

import Binary
import Inference
import Parser
import Loader
import Syntax
import Var

-- | /NB:/ when adding a test suite here, make sure you add it to
-- the @other-modules:@ block under @test-suite properties@ in
-- @ermine.cabal@ or you'll break @cabal sdist@.
main :: IO ()
main = defaultMain
  [ Binary.tests
  , Inference.tests
  , Loader.tests
  , Parser.tests
  , Syntax.tests
  , Var.tests
  ]
