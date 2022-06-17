{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

{- |
Module      : Database.Tagger.Query.Basic
Description : Contains basic queries.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Contains basic queries such as searching for files by file pattern or searching for
Descriptor by Relation.
-}
module Database.Tagger.Query (
  -- * Queries

  -- ** File Queries

  -- | Queries that return 'File` types.
  queryForUntaggedFiles,

  -- *** On 'File`

  -- | Queries that search based on some attribute of a 'File` type.
  queryForFileByPattern,
  queryForFileByExactPath,
  queryForFileByFileId,
  queryForSingleFileByFileId,

  -- *** On 'Descriptor`

  -- | Queries that search based on some attribute of a 'Descriptor` type.
  flatQueryForFileByTagDescriptor,

  -- * Operations
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Database.Tagger.Connection
import Database.Tagger.Query.Type
import Database.Tagger.Type
import Tagger.Util
import Text.RawString.QQ

{- |
 Performs a case-insensitive search of all registered file paths, tagged or not.

 Can use SQL wildcards like % or _
-}
queryForFileByPattern :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
queryForFileByPattern p tc = HashSet.fromList <$> query tc q [p]
 where
  q =
    [r|
      SELECT
        id
        ,filePath
      FROM
        File
      WHERE
        filePath LIKE ?
      |]

{- |
 Query for files based on exact matches of absolute file paths.

 More performant for bulk queries but harder to do manually.
-}
queryForFileByExactPath :: [T.Text] -> TaggedConnection -> IO (HashSet.HashSet File)
queryForFileByExactPath ps tc =
  HashSet.fromList <$> query tc (q . length $ ps) ps
 where
  q n =
    [r|
      SELECT
        id
        ,filePath
      FROM
        File
      WHERE
        filePath IN
      |]
      `qcat` paramList n

{- |
 Query for files by their ids.
-}
queryForFileByFileId :: [RecordKey] -> TaggedConnection -> IO (HashSet.HashSet File)
queryForFileByFileId rks tc =
  HashSet.fromList <$> query tc (q . length $ rks) rks
 where
  q n =
    [r|
      SELECT
        id
        ,filePath
      FROM
        File
      WHERE
        id IN
      |]
      `qcat` paramList n

{- |
 Query for a single file with its id.

 Probably more efficient if only one file is needed but if this needs to be run multiple
 times then it is better to use 'queryForFileByFileId`
-}
queryForSingleFileByFileId :: RecordKey -> TaggedConnection -> MaybeT IO File
queryForSingleFileByFileId rk tc = do
  result <- lift $ query tc q [rk] :: MaybeT IO [File]
  hoistMaybe . head' $ result
 where
  q =
    [r|
        SELECT
          id
          ,filePath
        FROM
          File
        WHERE
          id = ?
      |]

{- |
 Performs a case-insensitive search for files that are tagged with a descriptor
 matching the text pattern provided.

 A flat search, meaning that any descriptor that tags an image will be searched,
 regardless of whether or not it is a subtag or not.
-}
flatQueryForFileByTagDescriptor :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
flatQueryForFileByTagDescriptor p tc = HashSet.fromList <$> query tc q [p]
 where
  q =
    [r|
      SELECT
        f.id
        ,f.filePath
      FROM Tag t
        JOIN Descriptor d
          ON t.descriptorId = d.id
        JOIN File f
          ON t.fileId = f.id
      WHERE
        d.descriptor LIKE ?
      |]

{- |
 Query for files without tags.
-}
queryForUntaggedFiles :: TaggedConnection -> IO (HashSet.HashSet File)
queryForUntaggedFiles tc = HashSet.fromList <$> query_ tc q
 where
  q =
    [r|
      SELECT
        f.id
       ,f.filePath
      FROM File f
        LEFT JOIN Tag t
          ON f.id = t.fileId
      WHERE
        t.id IS NULL
      |]
