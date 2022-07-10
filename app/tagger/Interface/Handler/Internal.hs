{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Interface.Handler.Internal (
  Interface.Handler.Internal.renameFile,
  addFiles,
  shuffleSequence,
) where

import qualified Control.Exception as Exception
import Control.Monad (guard, when, (<=<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (
  ExceptT,
  runExceptT,
  throwE,
  withExceptT,
 )
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.Either as E
import Data.Hashable (Hashable, hashWithSalt)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  insertFiles,
  queryForSingleFileByFileId,
  updateFilePaths,
 )
import Database.Tagger.Type (
  File (File),
  RecordKey,
  TaggedConnection,
 )
import System.Directory as Directory (
  doesDirectoryExist,
  doesFileExist,
  doesPathExist,
  getCurrentDirectory,
  listDirectory,
  renameFile,
 )
import System.FilePath (makeRelative, (</>))
import System.IO (hPrint, hPutStrLn, stderr)
import System.Random

{- |
 Sorts a given sequence with a random seed.
-}
shuffleSequence :: Hashable a => Seq a -> IO (Seq a)
shuffleSequence s = do
  !shuffleSeed <- initStdGen :: IO StdGen
  let genFileHash = hashWithSalt (fst . random $ shuffleSeed)
      !sortedSeq = Seq.unstableSortOn genFileHash s
  return sortedSeq

{- |
 Renames a file in the database and file system at the same time.
 If the file does not exist in the db or it already exists in the file system,
 do nothing.

 If there is an error that occurs during renaming in the filesystem, revert the
 db name change.
-}
renameFile :: TaggedConnection -> RecordKey File -> Text -> IO ()
renameFile c fk (T.unpack -> newFilePath') = do
  result <- runExceptT $ renameFile' newFilePath'
  E.either (hPutStrLn stderr) pure result
 where
  renameFile' :: FilePath -> ExceptT String IO ()
  renameFile' newFilePath = do
    maybeDBFile <- lift . runMaybeT $ queryForSingleFileByFileId fk c
    (File fkFromDB (T.unpack -> oldFilePath)) <-
      maybe
        (throwE ("File with id, " ++ show fk ++ " not found in database."))
        return
        maybeDBFile
    withExceptT
      (const ("File, " ++ oldFilePath ++ " not found in filesystem."))
      . ((guard :: Bool -> ExceptT String IO ()) <=< lift . doesFileExist)
      $ oldFilePath
    withExceptT
      (const ("Path, " ++ newFilePath ++ " already exists in filesystem."))
      . ((guard . not :: Bool -> ExceptT String IO ()) <=< lift . doesPathExist)
      $ newFilePath
    lift $ updateFilePaths [(newFilePath, fkFromDB)] c
    fileSystemRenameResult <-
      lift . Exception.try $
        Directory.renameFile oldFilePath newFilePath ::
        ExceptT String IO (Either Exception.IOException ())
    when (E.isLeft fileSystemRenameResult) $ do
      let (E.Left ioEx) = fileSystemRenameResult
      lift $ hPrint stderr ioEx
      lift $ updateFilePaths [(oldFilePath, fkFromDB)] c
      throwE "Reverting database changes."

{- |
 Add all files recursively beginning at the given filepath to the database.

 The given path is made relative to the current working directory, then
  subsequent nested paths are made relative to that.
-}
addFiles :: TaggedConnection -> Text -> IO ()
addFiles c (T.unpack -> givenPath) = do
  curDir <- getCurrentDirectory
  let fpRelativeToCurDir = makeRelative curDir givenPath
  getPathsToAdd [] fpRelativeToCurDir >>= flip insertFiles c
 where
  getPathsToAdd :: [FilePath] -> FilePath -> IO [FilePath]
  getPathsToAdd acc fp = do
    pathIsDir <- doesDirectoryExist fp
    if pathIsDir
      then do
        dirContents <- listDirectory fp
        addedContents <- concat <$> mapM (\dp -> getPathsToAdd [] (fp </> dp)) dirContents
        return $ addedContents ++ acc
      else do
        pathIsFile <- doesFileExist fp
        if pathIsFile
          then return (fp : acc)
          else return acc
