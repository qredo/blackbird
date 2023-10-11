{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------

module Blackbird.Unification.Class
  ( ClassCheck(ClassCheck)
  , instantiateClass
  ) where

import Bound
import Bound.Scope
import Bound.Var
import Control.Lens
import Data.Map as Map
import Data.Text
import Data.Traversable
import Data.Void
import Blackbird.Syntax.Class
import Blackbird.Syntax.Global
import Blackbird.Syntax.Hint
import Blackbird.Syntax.Kind as Kind
import Blackbird.Syntax.Type as Type
import Blackbird.Syntax.Term as Term
import Blackbird.Unification.Meta
import GHC.Generics

data ClassCheck s = ClassCheck
                  { _cctparams :: [(Hint, KindM s)]
                  , _cccxt     :: [Scope Int (Typ (KindM s)) Text]
                  , _ccsigs    :: Map Global (Typ (KindM s) (Var Int Text))
                  , _ccdefs    :: Map Global (Bodies (Annot Void Text) Void)
                  }
  deriving (Eq, Show, Generic)

instantiateClass :: Class () Text -> M s (Schema (MetaK s), ClassCheck s)
instantiateClass cls = do
  clasz@(Class ks ts cxt sigs defs) <- kindVars (\_ -> newShallowMeta 0 False Nothing) cls
  mks <- for ks $ newMeta False
  tks <- for ts $ \(h, _) -> (,) h . pure <$> newShallowMeta 0 False Nothing
  return $ ( schema clasz
           , ClassCheck
               tks
               (hoistScope (over kindVars $ pure . unvar (mks!!) id) <$> cxt)
               (over kindVars (pure . unvar (mks!!) id) <$> sigs)
               defs
           )

