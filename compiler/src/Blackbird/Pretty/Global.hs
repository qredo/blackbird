--------------------------------------------------------------------
-- |
-- Copyright :  (c) Dan Doel 2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Pretty.Global
  ( prettyGlobal
  ) where

import Control.Lens
import Data.Text (unpack)
import Blackbird.Pretty
import Blackbird.Syntax.Global
import Blackbird.Syntax.Name

prettyGlobal :: Global -> Doc s
prettyGlobal (Global _ fix md nm) = case fix of
  Infix _ _ -> parens qualName
  Prefix  _ -> parens $ pretty "prefix" <+> qualName
  Postfix _ -> parens $ pretty "postfix" <+> qualName
  Idfix     -> qualName
 where
 qualName = pretty $ unpack (md^.name) ++ "." ++ unpack nm
