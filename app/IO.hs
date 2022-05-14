{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module IO
  ( module IO,
    module System.IO,
    module System.Environment,
    module System.Directory,
    module System.Process,
    module System.Random,
    module System.Random.Shuffle,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.Text as T
import Data.Time
import qualified Data.Version as Version
import Database.SQLite.Simple
import qualified Database.SQLite3 as DirectSqlite
import qualified Paths_tagger
import System.Directory
import System.Environment
import System.IO
import System.Process
import System.Random
import System.Random.Shuffle
import Toml
import Type.Config

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
  doesFileExist backupTo >>= flip unless (touch backupTo)
  updateLastBackupDateTime c
  backupHandle <- fmap connectionHandle . open $ backupTo
  backupProcess <- DirectSqlite.backupInit backupHandle "main" currentHandle "main"
  DirectSqlite.backupStep backupProcess (-1)
  DirectSqlite.backupFinish backupProcess
  hPutStrLn stderr "Backup complete"

updateTaggerDBInfo :: Connection -> IO ()
updateTaggerDBInfo c = do
  currentTime <- getCurrentTime
  dbInfoTableExists <-
    fmap
      (Prelude.all ((> 0) . (\(Only n) -> n)))
      ( query_
          c
          "SELECT COUNT(*) \
          \FROM sqlite_master \
          \WHERE type = 'table' AND name = 'TaggerDBInfo'" ::
          IO [Only Int]
      )
  when dbInfoTableExists $ do
    execute
      c
      "INSERT INTO TaggerDBInfo (_tagger, version, lastAccessed) \
      \VALUES (0, ?, ?)"
      (taggerVersion, currentTime)

updateLastBackupDateTime :: Connection -> IO ()
updateLastBackupDateTime c = do
  currentTime <- getCurrentTime
  execute c "UPDATE TaggerDBInfo SET lastBackup = ?" [currentTime]

taggerVersion :: String
taggerVersion = Version.showVersion Paths_tagger.version