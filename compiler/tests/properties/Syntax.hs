{-# LANGUAGE TemplateHaskell #-}

module Syntax where

import Blackbird.Syntax.Kind as Kind
import Blackbird.Syntax.Type as Type
import Blackbird.Syntax.Term as Term
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Arbitrary.SyntaxArbitrary ()

eq_reflexive :: Eq a => a -> Bool
eq_reflexive x = x == x

prop_kind_eq_reflexive :: Kind Int -> Bool
prop_kind_eq_reflexive = eq_reflexive

prop_type_eq_reflexive :: Typ Int Int -> Bool
prop_type_eq_reflexive = eq_reflexive

prop_term_eq_reflexive :: Term Int Int -> Bool
prop_term_eq_reflexive = eq_reflexive

tests = $testGroupGenerator
