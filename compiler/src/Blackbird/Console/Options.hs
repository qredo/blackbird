{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett and Dan Doel 2012-2013
-- License   :  BSD2
-- Maintainer:  Qredo LTD <support@qredo.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Blackbird.Console.Options
  ( MonitorOptions(..)
  , HasMonitorOptions(..)
  , Options(..)
  , HasOptions(..)
  , parseOptions
  ) where

import Control.Lens hiding (argument)
import Data.Data
import Blackbird.Monitor
import Options.Applicative
import Paths_blackbird

-- | All command line options.
data Options = Options
  { _optionsMonitorOptions :: MonitorOptions
  , _libdir :: FilePath
  , _files :: [FilePath]
  } deriving (Eq,Ord,Show,Read,Data)

makeClassy ''Options

instance HasMonitorOptions Options where
  monitorOptions = optionsMonitorOptions

-- | Generate the command line option parser
parseOptions :: IO (Parser Options)
parseOptions = do
  dd <- getDataDir
  return $ Options
       <$> parseMonitorOptions
       <*> option auto (long "libdir" <> short 'l' <> help "location of the ermine library" <> metavar "DIR" <> action "directory" <> value dd)
       <*> many (argument str $ help "files" <> metavar "FILE" <> action "file")
