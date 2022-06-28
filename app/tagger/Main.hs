{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad.Trans.Except
import Data.Config
import Data.Model
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger
import Interface
import System.Directory
import System.FilePath
import System.IO

main :: IO ()
main = do
  ec <- runExceptT getConfig
  either
    printConfigError
    withConfig
    ec

withConfig :: TaggerConfig -> IO ()
withConfig c = do
  workingDir <- getCurrentDirectory
  dbDir <- makeAbsolute . takeDirectory . T.unpack $ c ^. dbConf . path
  setCurrentDirectory dbDir
  runProgram c
  setCurrentDirectory workingDir

{- |
 Entry point for running the monomer program.
-}
runProgram :: TaggerConfig -> IO ()
runProgram c = do
  db <- openTaggedConnection $ c ^. dbConf
  runTagger (createTaggerModel c db (Descriptor (-1) ""))

openTaggedConnection :: DatabaseConfig -> IO TaggedConnection
openTaggedConnection c = open . T.unpack $ c ^. path

printConfigError :: Text -> IO ()
printConfigError e = do
  hPutStrLn stderr "Error while parsing config file:\n"
  T.IO.hPutStrLn stderr e
  hPutStrLn stderr "\nPlease use this as a template:\n"
  hPrintConf stderr exampleConf