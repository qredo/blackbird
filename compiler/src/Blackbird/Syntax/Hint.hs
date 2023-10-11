--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2013-2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------
module Blackbird.Syntax.Hint
  ( Hint
  ) where

import Data.Text (Text)

type Hint = Maybe Text
