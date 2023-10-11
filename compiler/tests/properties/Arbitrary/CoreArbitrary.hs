{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Arbitrary.CoreArbitrary where

import Arbitrary.Arbitrary
import Arbitrary.SyntaxArbitrary
import Data.Map
import Blackbird.Core.Module as Module
import Test.QuickCheck

-- Orphans
instance Arbitrary a => Arbitrary (Module a) where
  arbitrary = Module <$>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    smaller arbTypes <*>
    smaller dataTypes where
      arbTypes  = fmap fromList $ listOf ((,) <$> arbitrary <*> (genType Nothing Nothing))
      dataTypes = listOf (genDataType Nothing Nothing)
