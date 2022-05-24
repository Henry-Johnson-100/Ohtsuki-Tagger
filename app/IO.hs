{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module IO
  ( module IO,
    module System.IO,
    module System.Console.GetOpt,
    module System.Environment,
    module System.Directory,
    module System.FilePath,
    module System.Process,
    module System.Random,
    module System.Random.Shuffle,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time
import qualified Data.Version as Version
import Database.SQLite.Simple
import qualified Database.SQLite3 as DirectSqlite
import qualified Paths_tagger
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import System.Random
import System.Random.Shuffle
import Toml
import Type.Config
import Util.Core

type ConfigException = String

validateFilePath :: FilePath -> ExceptT ConfigException IO FilePath
validateFilePath p = do
  isFile <- lift . doesFileExist $ p
  if isFile then lift . makeAbsolute $ p else throwE $ "File not found: " ++ p

validateDirPath :: FilePath -> ExceptT ConfigException IO FilePath
validateDirPath p = do
  isDir <- lift . doesDirectoryExist $ p
  if isDir then lift . makeAbsolute $ p else throwE $ "Directory not found: " ++ p

getConfig :: String -> ExceptT String IO TaggerConfig
getConfig =
  withExceptT
    (T.unpack . prettyTomlDecodeErrors)
    . except
    <=< decodeFileEither taggerConfigCodec
    <=< validateFilePath

getConfigPath :: IO FilePath
getConfigPath = do
  userHome <- getEnv "HOME"
  return $ userHome ++ "/.config/tagger.toml"

getConfigDbConn :: FilePath -> ExceptT ConfigException IO FilePath
getConfigDbConn =
  withExceptT
    ("Configuration error when opening database connection:\n" ++)
    . validateFilePath

runInitScript :: FilePath -> Connection -> IO ()
runInitScript pathToScript c = do
  let cHandle = connectionHandle c
  initHandle <- openFile pathToScript ReadMode
  initContents <- hGetContents initHandle
  DirectSqlite.exec cHandle . T.pack $ initContents
  hClose initHandle

touch :: FilePath -> IO ()
touch = hClose <=< flip openFile WriteMode

backupDbConn :: Connection -> FilePath -> IO ()
backupDbConn c backupTo = do
  let currentHandle = connectionHandle c
  backupFileExists <- doesFileExist backupTo
  unless backupFileExists (touch backupTo)
  updateLastBackupDateTime c
  backupHandle <- fmap connectionHandle . open $ backupTo
  backupProcess <- DirectSqlite.backupInit backupHandle "main" currentHandle "main"
  DirectSqlite.backupStep backupProcess (-1)
  DirectSqlite.backupFinish backupProcess
  hPutStrLn stderr "Backup complete"

taggerDBInfoTableExists :: Connection -> IO Bool
taggerDBInfoTableExists c = do
  r <-
    query_
      c
      "SELECT COUNT(*) \
      \FROM sqlite_master \
      \WHERE type = 'table' AND name = 'TaggerDBInfo'" ::
      IO [Only Int]
  return . Prelude.all ((> 0) . (\(Only n) -> n)) $ r

updateTaggerDBInfo :: Connection -> IO ()
updateTaggerDBInfo c = do
  currentTime <- getCurrentTime
  dbInfoTableExists <- taggerDBInfoTableExists c
  when dbInfoTableExists $ do
    backupTimeString <- getLastBackupDateTime c
    execute
      c
      "UPDATE TaggerDBInfo SET version = ?, lastAccessed = ?"
      (taggerVersion, currentTime)

updateLastBackupDateTime :: Connection -> IO ()
updateLastBackupDateTime c = do
  currentTime <- getCurrentTime
  execute c "UPDATE TaggerDBInfo SET lastBackup = ?" [currentTime]

getLastAccessDateTime :: Connection -> IO T.Text
getLastAccessDateTime c = do
  infoTableExists <- taggerDBInfoTableExists c
  if infoTableExists
    then do
      r <-
        query_
          c
          "SELECT lastAccessed \
          \FROM TaggerDBInfo \
          \WHERE _tagger = 0" ::
          IO [Only (Maybe T.Text)]
      let hr = head' . mapMaybe (\(Only mt) -> mt) $ r
      return $ fromMaybe "NULL" hr
    else return "Not Available"

getLastBackupDateTime :: Connection -> IO T.Text
getLastBackupDateTime c = do
  infoTableExists <- taggerDBInfoTableExists c
  if infoTableExists
    then do
      r <-
        query_
          c
          "SELECT lastBackup \
          \FROM TaggerDBInfo \
          \WHERE _tagger = 0" ::
          IO [Only (Maybe T.Text)]
      let hr = head' . mapMaybe (\(Only mt) -> mt) $ r
      return $ fromMaybe "NEVER" hr
    else return "Not Available"

taggerVersion :: String
taggerVersion = Version.showVersion Paths_tagger.version