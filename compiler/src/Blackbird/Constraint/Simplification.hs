{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blackbird.Constraint.Simplification
  ( bySuper
  , bySupers
  , byInstance
  , entails
  , simplifyVia
  ) where

import Bound
import Control.Applicative
import Control.Lens
import Control.Monad hiding (sequence)
import Control.Monad.Trans.Maybe
import Data.List (delete)
import Data.Map as Map hiding (map, empty, delete, filter)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable
import Data.Void
import Blackbird.Constraint.Env
import Blackbird.Syntax
import Blackbird.Syntax.Convention
import Blackbird.Syntax.Core as Core hiding (App, Var)
import Blackbird.Syntax.Id
import Blackbird.Syntax.Head
import Blackbird.Syntax.Instance as Instance
import Blackbird.Syntax.Type as Type
import Prelude hiding (sequence)

mangle :: (Applicative f, Monad t, Traversable t) =>  (a -> f (t c)) -> t a -> f (t c)
mangle f c = join <$> traverse f c

-- Determines whether the first argument is above the second in the
-- instance hierarchy. Yields the Core to project out the superclass
-- dictionary.
--
-- We expect here that both arguments have been reduced by instance before
-- they arrive here.
bySuper :: (AsConvention cc, Alternative m, MonadConstraint cc s m, Eq k, Eq t)
        => Typ k t -> Typ k t -> m (Scope b (Core cc) (Typ k t))
bySuper c d = Prelude.foldr id (pure d) <$> go [] d
 where
 go ps c'
   | c == c'   = pure ps
   | otherwise = superclasses c' >>= asum . zipWith (\i -> go (super i:ps)) [0..]

-- As bySuper.
bySupers :: (AsConvention cc, Alternative m, MonadConstraint cc s m, Eq k, Eq t)
         => Typ k t -> [Typ k t] -> m (Scope b (Core cc) (Typ k t))
bySupers c cs = asum (map (bySuper c) cs)
                      >>= mangle (\d ->
                              d `bySupers` delete d cs <|>
                              pure (pure d))

-- Checks if a series of type arguments would match against the type
-- type \"patterns\" of an instance head. Returns a mapping from the
-- variables bound in the instance to the types they would correspond to
-- after the match.
--
-- Basic equality is used, as no unification should be caused by this
-- action. This means that the types should be fully zonked when passed in,
-- or the results will be erroneous.
matchHead :: (Eq k, Eq t) => [Typ k t] -> Head -> Maybe (Map Int (Typ k t))
matchHead ts he = join . fmap (traverse check . fromListWith (++) . join)
                . sequence $ zipWith matchArg (he^.headTypeArgs) ts
 where
 check [x]        = Just x
 check (x:xs)
   | all (x==) xs = Just x
 check _          = Nothing

 matchArg (Var i)      t                       = Just [(i, [t])]
 matchArg (App f x)    (App g y)               = (++) <$> matchArg f g <*> matchArg x y
 matchArg (HardType h) (HardType h') | h == h' = Just []
 matchArg _            _                       = Nothing

byInstance
  :: (AsConvention cc, Eq k, Eq t, Alternative m, MonadConstraint cc s m)
  => Typ k t -> m (Scope b (Core cc) (Typ k t))
byInstance = peel []
 where
 peel stk (App f x) = peel (x:stk) f
 peel stk (HardType (Con g _)) = viewConstraint (instances . at g) >>= \ xs -> case xs of
   Nothing -> empty
   Just [] -> empty
   Just is -> case catMaybes $ tryConstraint stk <$> is of
     []  -> empty
     [x] -> pure x
     _   -> fail "Ambiguous class discharge"
 peel _   _ = error "PANIC: Malformed class head"

 tryConstraint stk i = matchHead stk (i^.instanceHead) <&> \m ->
   Prelude.foldl (\ r a -> _AppDict # (r, pure $ join $ bimap absurd (m!) a)) (_Id._InstanceId # (i^.instanceHead)) $ i^.instanceContext

entails :: (AsConvention cc, Alternative m, MonadConstraint cc s m, Eq k, Eq t)
        => [Typ k t] -> Typ k t -> m (Scope b (Core cc) (Typ k t))
entails cs c =
  c `bySupers` cs' <|> (byInstance c >>= simplifyVia cs)
 where cs' = filter (/= c) cs

simplifyVia
  :: (AsConvention cc, MonadConstraint cc s m, Eq k, Eq t)
  => [Typ k t] -> Scope b (Core cc) (Typ k t) -> m (Scope b (Core cc) (Typ k t))
simplifyVia cs s = do
  x <- for s $ \t -> runMaybeT (entails cs t) <&> fromMaybe (pure t)
  return $ join x
