--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
----------------------------------------------------------------------
module Blackbird.Syntax.Name
  ( HasName(..)
  ) where

import Control.Lens
import Data.Text

class HasName t where
  name :: Lens' t Text
