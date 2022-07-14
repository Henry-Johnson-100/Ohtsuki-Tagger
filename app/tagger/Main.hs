{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import Control.Monad (guard, when)
import Control.Monad.Trans.Except (
  ExceptT,
  runExceptT,
  throwE,
  withExceptT,
 )
import Data.Model.Core (
  createTaggerModel,
  focusedFileDefaultDataFile,
 )
import qualified Data.Text as T
import Data.Version (showVersion)
import Database.Tagger (Descriptor (Descriptor), open)
import Interface (runTagger)
import Paths_tagger (getDataFileName)
import System.Console.GetOpt (
  ArgDescr (NoArg, ReqArg),
  ArgOrder (ReturnInOrder),
  OptDescr (..),
  getOpt,
 )
import System.Directory (
  getCurrentDirectory,
  makeAbsolute,
  setCurrentDirectory,
 )
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import Tagger.Info (taggerVersion)

data ProgArg
  = ProgVersion
  | ProgDBPath FilePath
  deriving (Show, Eq)

options :: [OptDescr ProgArg]
options =
  [ Option
      "vV"
      ["version"]
      (NoArg ProgVersion)
      "Show the version."
  , Option
      "pP"
      ["path"]
      (ReqArg ProgDBPath "PATH")
      "The path to the tagger database."
  ]

argOrder :: ArgOrder ProgArg
argOrder = ReturnInOrder ProgDBPath

main :: IO ()
main = do
  argString <- getArgs
  let p@(opts, _, _) =
        getOpt argOrder options argString
  when
    (ProgVersion `elem` opts)
    (putStrLn . showVersion $ taggerVersion)
  eitherDBPath <- runExceptT $ getConnString p
  either (hPutStrLn stderr) withDBPath eitherDBPath
 where
  head' [] = Nothing
  head' (x : _) = Just x

  getConnString ::
    ([ProgArg], [String], [String]) ->
    ExceptT String IO ProgArg
  getConnString (opts, nonOpts, errors) = do
    withExceptT
      (const ("Unknown options: " ++ unlines nonOpts))
      (guard (null nonOpts) :: ExceptT String IO ())
    withExceptT
      (const ("Errors reading options: " ++ unlines errors))
      (guard (null errors) :: ExceptT String IO ())
    maybe
      (throwE "No database path specified, use '-p PATH'")
      return
      ( head'
          . filter
            (\x -> case x of (ProgDBPath _) -> True; _ -> False)
          $ opts
      )

withDBPath :: ProgArg -> IO ()
withDBPath (ProgDBPath p) = do
  workingDir <- getCurrentDirectory
  dbDir <- makeAbsolute . takeDirectory $ p
  setCurrentDirectory dbDir
  runProgram p
  setCurrentDirectory workingDir
withDBPath _ =
  hPutStrLn
    stderr
    "Something went wrong with the given database path option."

{- |
 Entry point for running the monomer program.
-}
runProgram :: FilePath -> IO ()
runProgram p = do
  db <- open p
  defaultFile <- T.pack <$> getDataFileName focusedFileDefaultDataFile
  runTagger
    ( createTaggerModel
        db
        (Descriptor (-1) "fake descriptor")
        (Descriptor (-2) "fake #UNRELATED#")
        defaultFile
    )
