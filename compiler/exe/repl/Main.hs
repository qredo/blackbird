module Main ( main ) where

import Control.Exception.Lens
import Control.Lens
import Control.Monad.State.Strict
import Data.ByteString.Lens
import Data.Char
import Data.Default
import Options.Applicative
import System.Console.Haskeline
import System.Exit.Lens

import Blackbird.Console.Command
import Blackbird.Console.Completion
import Blackbird.Console.Options
import Blackbird.Console.State
import Blackbird.Console.Unicode
import Blackbird.Monitor
import Blackbird.Version

main :: IO ()
main = withUnicode $ do
  optionParser <- parseOptions

  opts <- execParser $ info (helper <*> optionParser) $
    fullDesc
    <> progDesc "The blackbird policy language"
    <> header ("blackbird " ++ version)

  putStrLn logo

  server <- if opts^.monitorEnabled
    then do
      putStrLn $ "Monitoring enabled at http://" ++ opts^.monitorHost ++ ":" ++ show (opts^.monitorPort) ++ "/"
      Just <$> forkServer (opts^.monitorHost.packedChars) (opts^.monitorPort)
    else pure Nothing

  evalStateT ?? def $ runInputT settings $ do
    loop server

loop :: Maybe Server -> InputT Console ()
loop server = do
  minput <- getInputLine ">> "
  case Prelude.dropWhile isSpace <$> minput of
    Nothing      -> return ()
    Just "quit"  -> return ()
    Just (':':cmd) -> do
      lift $ handling (filtered (hasn't _ExitCode)) (liftIO . print) (executeCommand cmd)
      loop server
    Just ""      -> loop server
    Just input   -> do
      outputStrLn $ "Input was: " ++ input
      loop server
