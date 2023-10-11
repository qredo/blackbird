{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inference where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST.Class
import Data.Maybe
import Data.Map as Map
import Blackbird.Builtin.Type
import Blackbird.Constraint.Env
import Blackbird.Constraint.Simplification
import Blackbird.Unification.Meta
import Blackbird.Syntax
import Blackbird.Syntax.Convention (Convention)
import Blackbird.Syntax.Class
import Blackbird.Syntax.Core hiding (App)
import Blackbird.Syntax.Global
import Blackbird.Syntax.Head
import Blackbird.Syntax.Instance
import Blackbird.Syntax.Kind as Kind
import Blackbird.Syntax.ModuleName
import Blackbird.Syntax.Type (Typ(App), con, trivialTK)
import Test.QuickCheck
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

newtype DD s a = DD { unDD :: M s (Maybe a) }

fooCon, barCon, bazCon :: Typ k t
fooCon = con foo (constraint ~> star)
barCon = con bar (constraint ~> star)
bazCon = con baz (constraint ~> star)

foo, bar, baz :: Global
foo = glob Idfix (mkModuleName_ "Blackbird") "Foo"
bar = glob Idfix (mkModuleName_ "Blackbird") "Bar"
baz = glob Idfix (mkModuleName_ "Blackbird") "Baz"

instance MonadST (DD s) where
  type World (DD s) = s
  liftST = DD . liftST . fmap Just

instance Monad (DD s) where
  DD m >>= f = DD $ do
    ma <- m
    case ma of
      Just x -> unDD $ f x
      Nothing -> return Nothing

instance MonadFail (DD s) where
  fail msg = DD $ fail msg

instance Functor (DD s) where
  fmap = liftM

instance Applicative (DD s) where
  pure = DD . return . return
  (<*>) = ap

instance MonadMeta s (DD s) where
  askMeta = DD $ Just <$> askMeta
  localMeta f (DD m) = DD $ localMeta f m

instance Alternative (DD s) where
  empty = DD $ pure Nothing
  DD mma <|> dd = DD $ mma >>= \ma -> case ma of
    Nothing -> unDD dd
    _       -> pure ma

fooInstance = Instance [] (mkHead foo 0 [] [] [int]) (Dict [] [])
barInstance = Instance [] (mkHead bar 0 [] [] [int]) (Dict [fooInstance^.instanceBody] [])
bazInstance = Instance [] (mkHead baz 0 [] [] [int]) (Dict [barInstance^.instanceBody] [])

dumbConstraintEnv = ConstraintEnv
                 { _classes = Map.fromList
                           [ (foo, Class [] [(Nothing, Scope star)] [] Map.empty Map.empty)
                           , (baz, Class [] [(Nothing, Scope star)] [view (from trivialTK) $ barCon `App` pure 0] Map.empty Map.empty)
                           , (bar, Class [] [(Nothing, Scope star)] [view (from trivialTK) $ fooCon `App` pure 0] Map.empty Map.empty)
                           ]
                 , _instances = Map.fromList
                     [ (foo, [fooInstance])
                     , (bar, [barInstance])
                     , (baz, [bazInstance])
                     ]
                 }

instance MonadConstraint Convention s (DD s) where
  type Cv (DD s) = Convention
  askConstraint = return dumbConstraintEnv
  localConstraint _ m = m

runDD :: (forall s. DD s a) -> Maybe a
runDD dd = either (\_ -> Nothing) id $ runM mempty (unDD dd)

prop_discharge_optimal = let v = pure () in
  forAll (oneof [ pure [App barCon v, App bazCon v]
                , pure [App bazCon v, App barCon v]
                ]) $ \sups ->
    fromMaybe False $ runDD $ do
      c :: Scope () (Core Convention) (Typ () ()) <- App fooCon v `bySupers` sups
      return $ c == super 0 (super 0 . pure $ App bazCon v)

prop_entails_optimal = let v = pure () in
  forAll (oneof [ pure [App barCon v, App bazCon v]
                , pure [App bazCon v, App barCon v]
                ]) $ \sups ->
    fromMaybe False $ runDD $ do
      c :: Scope () (Core Convention) (Typ () ()) <- sups `entails` App fooCon v
      return $ c == super 0 (super 0 . pure $ App bazCon v)

prop_instance_discharge = maybe False (const True) $ runDD $ do
  byInstance (App fooCon int :: Typ () ())

tests = $testGroupGenerator
