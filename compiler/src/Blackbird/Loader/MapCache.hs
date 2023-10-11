{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) McGraw Hill Financial 2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A Loader wrapper that caches in a Map
--------------------------------------------------------------------

module Blackbird.Loader.MapCache
  ( cacheLoad
  , loadCached
  ) where

import Control.Lens
import Control.Monad.State
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import Blackbird.Loader.Core

-- | Load from cache if possible, freshly otherwise.
cacheLoad :: (Hashable a, Monad m)
          => Loader e m a b -> HashMap a (e, b) -> a -> m b
cacheLoad l m a = snd `liftM`
  maybe (view load l a)
        (\eb@(e, _) ->
          fromMaybe eb `liftM` view reload l a e)
        (HM.lookup a m)

-- | Load using cache embedded in the loader's effect.
loadCached :: (Hashable a, MonadState (HashMap a (e, b)) m)
           => Loader e m a b -> a -> m b
loadCached l a = get >>= flip (cacheLoad l) a
