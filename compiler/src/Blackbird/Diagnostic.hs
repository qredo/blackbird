{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module re-exports 'Rendering' from @trifecta@ along with
-- our in-house diagnostic type.
--
-- This allows the non-parsing modules to not have to concern
-- themselves with how we choose to represent source locations.
--------------------------------------------------------------------
module Blackbird.Diagnostic
  (
  -- * Rendering
    Rendering
  , HasRendering(..)
  -- * Diagnostics
  , Diagnostic(Diagnostic)
  , AsDiagnostic(..)
  , HasDiagnostic(..)
  , Err(Err)
  , HasErr(..)
  , die
  , diePretty
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Data.Set
import Data.Text
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Read
import Text.Trifecta.Rendering
import Text.Trifecta.Result

-- | Ermine diagnostic type
data Diagnostic = Diagnostic
  { _diagnosticRendering :: !Rendering
  , _diagnosticMessage   :: Maybe (Doc AnsiStyle)
  , _diagnosticNotes     :: [Doc AnsiStyle]
  , _diagnosticExpected  :: Set String
  }

makeClassy ''Diagnostic

-- | Construct a diagnostic
die :: HasRendering r => r -> Text -> Diagnostic
die r s = Diagnostic (r^.rendering) (Just (pretty $ unpack s)) [] mempty

-- | Construct a fancy diagnostic
diePretty :: HasRendering r => r -> Doc AnsiStyle -> Diagnostic
diePretty r s = Diagnostic (r^.rendering) (Just s) [] mempty

-- | This provides the '_Diagnostic' prism that can be used when matching against 'SomeException'
-- or in a custom 'Error' type.
class AsDiagnostic t where
  _Diagnostic :: Prism' t Diagnostic

instance AsDiagnostic Diagnostic where
  _Diagnostic = id

instance AsDiagnostic SomeException where
  _Diagnostic = exception

instance Exception Diagnostic

instance Show Diagnostic where
  show d = show $ unAnnotate $ explain (d^.rendering) (d^.err)

instance HasErr Diagnostic where
  err f (Diagnostic r p a x) = f (Err p a x []) <&> \(Err q b y _) -> Diagnostic r q b y

instance HasRendering Diagnostic where
  rendering = diagnosticRendering

instance Read Rendering where
  readPrec = pfail
