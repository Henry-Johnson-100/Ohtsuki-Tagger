{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Interface.Handler.Internal (
  addFiles,
  shuffleSequence,
) where

import Data.Hashable (Hashable, hashWithSalt)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  insertFiles,
 )
import Database.Tagger.Type (
  TaggedConnection,
 )
import System.Directory as Directory (
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  listDirectory,
 )
import System.FilePath (makeRelative, (</>))
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
