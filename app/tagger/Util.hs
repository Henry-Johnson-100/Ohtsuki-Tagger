{-# LANGUAGE ViewPatterns #-}

module Util (
  head',
  both,
  addFiles,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (TaggedConnection, insertFiles)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  listDirectory,
 )
import System.FilePath (makeRelative, (</>))

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = Just . head $ xs

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)

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
