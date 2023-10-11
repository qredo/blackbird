{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012, Qredo Ltd 2023
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Pretty.Meta
  (
  -- * Pretty Printing Kinds
    prettyMeta
  , prettyMetaK
  , prettyKindM
  , prettyMetaT
  , prettyTypeM
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (fromMaybe)
import Blackbird.Pretty
import Blackbird.Unification.Meta
import Blackbird.Pretty.Kind (prettyKindF)
import Blackbird.Pretty.Type

-- $setup
-- >>> :set -XOverloadedStrings

-- | Pretty print a 'Kind'
prettyMeta :: MonadMeta s m => (a -> Int -> m (Doc ann)) -> (f (Meta s f a) -> Int -> m (Doc ann)) -> Meta s f a -> Int -> m (Doc ann)
prettyMeta pa pf m prec =
  readMeta m >>= \mv -> case mv of
    Nothing  -> (\da -> parensIf (prec >= 0) $ (pretty . fromMaybe "_" $ m ^. metaHint) <+> ":" <+> da) <$> pa (m ^. metaValue) (-1)
    Just fmf -> pf fmf prec
{-# INLINE prettyMeta #-}

prettyMetaK :: MonadMeta s m => MetaK s -> Int -> m (Doc ann)
prettyMetaK = prettyMeta (flip (const (pure . pretty))) prettyKindM

prettyKindM :: MonadMeta s m => KindM s -> Int -> m (Doc ann)
prettyKindM = prettyKindF prettyMetaK

prettyMetaT :: MonadMeta s m => MetaT s -> [String] -> Int -> m (Doc ann)
prettyMetaT m vs d = runReaderT (prettyMeta prettyKindM (\t d' -> ReaderT $ \vs' -> prettyTypeM t vs' d') m d) vs

prettyTypeM :: MonadMeta s m => TypeM s -> [String] -> Int -> m (Doc ann)
prettyTypeM = prettyTypeF (\m _ d -> prettyMetaK m d) prettyMetaT
