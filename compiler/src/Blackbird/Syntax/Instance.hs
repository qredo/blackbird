{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-} -- for HasHead Instance
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Syntax.Instance
  ( Instance(..)
  , HasInstance(..)
  ) where

import Control.Lens
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Functor.Classes (Eq1(liftEq),liftEq2)
import Data.Hashable
import Data.Hashable.Lifted
import Data.Void
import Blackbird.Syntax.Core
import Blackbird.Syntax.Head
import Blackbird.Syntax.Id
import Blackbird.Syntax.Type
import GHC.Generics

------------------------------------------------------------------------------
-- Instance
------------------------------------------------------------------------------

-- instance Ord a => Ord [a]
-- Instance [ord (pure 0)] (Head ord 0 [star] [] [list (pure 0)]) (LamDict (Scope (Dict [AppDict (InstanceId (Head eq 0 [star] [] [list (pure 0))) (AppDict (Slot 0) (B ()))] ...)))

-- instance Category (:-)
-- Instance [] (Head category 0 [] [constraint] [con ":-" ...]) (Dict [] ...)

data Instance c = Instance
  { _instanceContext :: [Typ Void Int]
  , _instanceHead :: Head
  , _instanceBody :: Core c Id
  } deriving (Eq, Generic, Show)

class HasInstance t c | t -> c where
  instance_ :: Lens' t (Instance c)
  instanceBody :: Lens' t (Core c Id)
  instanceContext :: Lens' t [Typ Void Int]
  instanceHead :: Lens' t Head

  default instanceBody :: HasInstance (Instance c) c => Lens' t (Core c Id)
  instanceBody = instance_.instanceBody
  default instanceContext :: HasInstance (Instance c) c => Lens' t [Typ Void Int]
  instanceContext = instance_.instanceContext
  default instanceHead :: HasInstance (Instance c) c => Lens' t Head
  instanceHead = instance_.instanceHead

makeLensesWith ?? ''Instance $ classyRules & createClass .~ False & lensClass .~ \_ -> Just (''HasInstance, 'instance_)

instance Eq1 Instance where
  liftEq f (Instance c h b) (Instance c' h' b') = c == c' && h == h' && liftEq2 f (==) b b'

instance Hashable a => Hashable (Instance a) where
  hashWithSalt = liftHashWithSalt hashWithSalt
instance Hashable1 Instance where
  liftHashWithSalt h n (Instance ctx hd bd) =
    hashWithSalt n ctx `hashWithSalt` hd `hcws` bd
   where
   infixl 0 `hcws`
   hcws n' c = liftHashWithSalt2 h hashWithSalt n' c

instance Functor Instance where
  fmap f (Instance c h b) = Instance c h $ first f b

instance Foldable Instance where
  foldMap f (Instance _ _ b) = bifoldMap f (const mempty) b

instance Traversable Instance where
  traverse f (Instance cxt hd b) = Instance cxt hd <$> bitraverse f pure b
