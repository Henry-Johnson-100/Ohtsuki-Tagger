{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

import Control.Monad (unless, when, (<=<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
  ( ExceptT,
    except,
    throwE,
    withExceptT,
  )
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.Version as Version
import Database.SQLite.Simple
  ( Connection (connectionHandle),
    Only (Only),
    execute,
    open,
    query_,
  )
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
import Toml (decodeFileEither, prettyTomlDecodeErrors)
import Type.Config (TaggerConfig, taggerConfigCodec)
import Util.Core (head')

class Show e => Exception e where
  liftEx :: String -> e
  exMsg :: e -> String
  exMsg = show

  -- Change kind of exception
  eLabel :: Exception e1 => (String -> e1) -> e -> e1
  emap :: (String -> String) -> e -> e
  emap f = liftEx . f . exMsg

newtype ConfigException = ConfigException String deriving (Eq)

instance Show ConfigException where
  show (ConfigException m) = "Configuration exception: " ++ m

instance Exception ConfigException where
  liftEx = ConfigException
  eLabel f (ConfigException m) = f m

instance Exception String where
  liftEx = id
  eLabel f s = f s

guardException :: (Monad m, Exception e) => String -> Bool -> ExceptT e m ()
guardException msg b = unless b $ throwE . liftEx $ msg

maybeException :: (Monad m, Exception e) => String -> MaybeT m a -> ExceptT e m a
maybeException msg m = do
  x <- lift . runMaybeT $ m
  maybe (throwE . liftEx $ msg) return x

-- |
-- Throw exception if the given file path does not point to a file.
guardFileExists :: Exception e => [Char] -> ExceptT e IO ()
guardFileExists p =
  guardException ("File \"" ++ p ++ "\" does not exist")
    <=< lift . doesFileExist
    $ p

-- |
-- Throw exception if the given path points to a file.
--
-- Used before copying or renaming to ensure the destination is not already occupied.
guardFileDoesNotExist :: Exception e => [Char] -> ExceptT e IO ()
guardFileDoesNotExist p =
  guardException ("File \"" ++ p ++ "\" Already exists")
    <=< fmap not . lift . doesFileExist
    $ p

guardDirectoryExists :: Exception e => [Char] -> ExceptT e IO ()
guardDirectoryExists p =
  guardException ("Directory \"" ++ p ++ "\" Does not exist")
    <=< lift . doesDirectoryExist
    $ p

validateFilePath :: FilePath -> ExceptT ConfigException IO FilePath
validateFilePath p = do
  isFile <- lift . doesFileExist $ p
  if isFile
    then lift . makeAbsolute $ p
    else
      throwE . ConfigException $
        "File not found: " ++ p

validateDirPath :: FilePath -> ExceptT ConfigException IO FilePath
validateDirPath p = do
  isDir <- lift . doesDirectoryExist $ p
  if isDir
    then lift . makeAbsolute $ p
    else throwE . ConfigException $ "Directory not found: " ++ p

getConfig :: FilePath -> ExceptT ConfigException IO TaggerConfig
getConfig p = do
  validatedPath <- validateFilePath p
  decoded <- decodeFileEither taggerConfigCodec validatedPath
  withExceptT (liftEx . T.unpack . prettyTomlDecodeErrors) . except $ decoded

getConfigPath :: IO FilePath
getConfigPath = do
  userHome <- getEnv "HOME"
  return $ userHome ++ "/.config/tagger.toml"

getConfigDbConn :: FilePath -> ExceptT ConfigException IO FilePath
getConfigDbConn =
  withExceptT
    (emap ("Configuration error when opening database connection:\n" ++))
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
  _ <- DirectSqlite.backupStep backupProcess (-1)
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
    _ <- getLastBackupDateTime c
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

renameFileSystemFile :: T.Text -> T.Text -> IO ()
renameFileSystemFile p to = do
  let pPath = T.unpack p
      toPath = T.unpack to
  renameFile pPath toPath

deleteFileSystemFile :: T.Text -> IO ()
deleteFileSystemFile = removeFile . T.unpack