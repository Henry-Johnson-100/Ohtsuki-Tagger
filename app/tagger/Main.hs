{-# LANGUAGE OverloadedStrings #-}

import Data.Model
import qualified Data.Text as T
import Database.Tagger
import Interface
import Paths_tagger
import System.Directory
import System.FilePath

main :: IO ()
main = withConfig

withConfig :: IO ()
withConfig = do
  workingDir <- getCurrentDirectory
  dbDir <- makeAbsolute . takeDirectory . T.unpack $ ""
  setCurrentDirectory dbDir
  runProgram
  setCurrentDirectory workingDir

{- |
 Entry point for running the monomer program.
-}
runProgram :: IO ()
runProgram = do
  db <- openTaggedConnection
  defaultFile <- T.pack <$> getDataFileName focusedFileDefaultDataFile
  runTagger
    ( createTaggerModel
        db
        (Descriptor (-1) "fake descriptor")
        (Descriptor (-2) "fake #UNRELATED#")
        defaultFile
    )

openTaggedConnection :: IO TaggedConnection
openTaggedConnection = open ""
