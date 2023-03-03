{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Database.Tagger
Description : Exports Database operations.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.Tagger (
  module Database.Tagger.Connection,
  module Database.Tagger.Query,
  module Database.Tagger.Type,
  rmFile,
  mvFile,
) where

import qualified Control.Exception as Exception
import Control.Monad (guard, when, (<=<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (
  ExceptT,
  except,
  runExceptT,
  throwE,
  withExceptT,
 )
import qualified Data.Either as E
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Connection
import Database.Tagger.Query
import Database.Tagger.Type
import System.Directory (
  doesFileExist,
  doesPathExist,
  removeFile,
  renameFile,
 )
import System.IO (hPrint, hPutStrLn, stderr)

{- |
 Delete a 'File` from the database and filesystem.

 An error will be printed if the 'File` does not exist in the local filesystem,
 but the given 'File` is always removed from the database anyways.
-}
rmFile :: TaggedConnection -> RecordKey File -> IO ()
rmFile tc fk = do
  result <- runExceptT $ do
    (File dbFk (T.unpack -> fp)) <- guardFileInDatabase tc fk
    lift $ deleteFiles [dbFk] tc
    rmResult <-
      lift . Exception.try $ removeFile fp ::
        ExceptT String IO (Either Exception.IOException ())
    withExceptT show . except $ rmResult
  either (hPutStrLn stderr) return result

{- |
 Renames a file in the database and file system at the same time.
 If the file does not exist in the db or it already exists in the file system,
 do nothing.

 If there is an error that occurs during renaming in the filesystem, revert the
 db name change.
-}
mvFile :: TaggedConnection -> RecordKey File -> Text -> IO ()
mvFile c fk (T.unpack -> newFilePath') = do
  result <- runExceptT $ renameFile' newFilePath'
  E.either (hPutStrLn stderr) pure result
 where
  renameFile' :: FilePath -> ExceptT String IO ()
  renameFile' newFilePath = do
    (File fkFromDB (T.unpack -> oldFilePath)) <- guardFileInDatabase c fk
    guardFileExists oldFilePath
    withExceptT
      (const ("Path, " ++ newFilePath ++ " already exists in filesystem."))
      . ((guard . not :: Bool -> ExceptT String IO ()) <=< lift . doesPathExist)
      $ newFilePath
    lift $ updateFilePaths [(newFilePath, fkFromDB)] c
    fileSystemRenameResult <-
      lift . Exception.try $
        renameFile oldFilePath newFilePath ::
        ExceptT String IO (Either Exception.IOException ())
    when (E.isLeft fileSystemRenameResult) $ do
      let (E.Left ioEx) = fileSystemRenameResult
      lift $ hPrint stderr ioEx
      lift $ updateFilePaths [(oldFilePath, fkFromDB)] c
      throwE "Reverting database changes."

guardFileExists :: FilePath -> ExceptT String IO ()
guardFileExists p =
  withExceptT
    (const ("File, " ++ p ++ " not found in filesystem."))
    . ((guard :: Bool -> ExceptT String IO ()) <=< lift . doesFileExist)
    $ p

guardFileInDatabase :: TaggedConnection -> RecordKey File -> ExceptT String IO File
guardFileInDatabase c fk = do
  maybeDBFile <- lift $ queryForSingleFileByFileId fk c
  maybe
    (throwE ("File with id, " ++ show fk ++ " not found in database."))
    return
    maybeDBFile