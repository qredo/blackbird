--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2023 Qredo Ltd
-- License   :  Apache 2.0
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the parser for policies
--------------------------------------------------------------------
module Blackbird.Parser.Policy
  ( policy
  ) where

import Control.Monad.State (MonadState)
import Data.Text (Text)
import Text.Parser.Combinators (endBy)
import Text.Parser.Token (semi)

import Blackbird.Builtin.Term (let_)
import Blackbird.Parser.Type as Type
import Blackbird.Parser.Term (declarations', declClause, term)
import Blackbird.Parser.Trifecta (LayoutParsing(layouted))
import Blackbird.Syntax.Module (HasFixities)
import Blackbird.Syntax.Term (Term)

policy :: (MonadFail m, MonadState s m, HasFixities s, LayoutParsing m) => m (Term Ann Text)
policy = layouted $ let_ <$> declarations' (endBy declClause semi) <*> term
