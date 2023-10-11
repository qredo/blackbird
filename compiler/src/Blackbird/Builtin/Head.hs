
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2014
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--------------------------------------------------------------------

module Blackbird.Builtin.Head
  ( hlameI
  , hlameL
  , hfromIntegerI
  , hfromIntegerL
  ) where

import Blackbird.Builtin.Type
import Blackbird.Syntax.Head

hlameI, hlameL :: Head
hlameI = mkHead lame 0 [] [] [int]
hlameL = mkHead lame 0 [] [] [long]

hfromIntegerI, hfromIntegerL :: Head
hfromIntegerI = mkHead fromInteg 0 [] [] [int]
hfromIntegerL = mkHead fromInteg 0 [] [] [long]
