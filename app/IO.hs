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
import Database.SQLite.Simple
import qualified Database.SQLite3 as DirectSqlite
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

-- initializeDatabase :: FilePath -> FilePath -> p -> IO ()
-- initializeDatabase scriptLoc dbLoc backupLoc = do
--   memConn <- open ":memory:"
--   let memConnHandle = connectionHandle memConn
--   scriptHandle <- openFile scriptLoc ReadMode
--   scriptContents <- hGetContents scriptHandle
--   newDbConn <- do
--     touch dbLoc
--     open dbLoc
--   DirectSqlite.exec memConnHandle . T.pack $ scriptContents
--   hClose scriptHandle
--   newDbSave <-
--     DirectSqlite.backupInit
--       (connectionHandle newDbConn)
--       "main"
--       memConnHandle
--       "main"
--   DirectSqlite.backupStep newDbSave (-1)
--   DirectSqlite.backupFinish newDbSave
--   close memConn
--   close newDbConn

touch :: FilePath -> IO ()
touch = hClose <=< flip openFile WriteMode

backupDbConn :: Connection -> FilePath -> IO ()
backupDbConn c backupTo = do
  let currentHandle = connectionHandle c
  doesFileExist backupTo >>= flip unless (touch backupTo)
  backupHandle <- fmap connectionHandle . open $ backupTo
  backupProcess <- DirectSqlite.backupInit backupHandle "main" currentHandle "main"
  DirectSqlite.backupStep backupProcess (-1)
  DirectSqlite.backupFinish backupProcess
  hPutStrLn stderr "Backup complete"
