{-# LANGUAGE DeriveDataTypeable #-}
module Blackbird.Monitor.Exception
  ( ShutdownMonitor(..)
  ) where

import Control.Exception
import Data.Data

data ShutdownMonitor = ShutdownMonitor deriving (Show,Data)

instance Exception ShutdownMonitor
