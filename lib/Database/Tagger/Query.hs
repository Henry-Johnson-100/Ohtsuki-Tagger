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
  -- $Operations

  -- ** 'File` Operations
  insertFiles,
  deleteFiles,

  -- ** 'Descriptor` Operations
  insertDescriptors,

  -- ** 'Relation` Operations
  insertDescriptorRelation,
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger.Connection
import Database.Tagger.Query.Type
import Database.Tagger.Type
import System.IO
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

{- $Operations
 functions that perform some action on the database like
 inserting new records or creating relations.
-}

{- |
 Insert a list of given 'FilePath`s as 'File` records into the database.

 Does not validate or resolve file paths.
-}
insertFiles :: [FilePath] -> TaggedConnection -> IO ()
insertFiles ps tc =
  executeMany tc q (map Only ps)
 where
  q =
    [r|
    INSERT INTO File (filePath) VALUES (?)
    |]

{- |
 Given a list of exact absolute paths, delete their corresponding records from
 the database.
-}
deleteFiles :: [FilePath] -> TaggedConnection -> IO ()
deleteFiles ps tc = executeMany tc q (Only <$> ps)
 where
  q =
    [r|
    DELETE FROM File WHERE filePath = ?
    |]

{- |
 Given a list of labels, create new 'Descriptor` rows in the database.

 The new 'Descriptor`s will automatically be related to \#UNRELATED\#.
-}
insertDescriptors :: [T.Text] -> TaggedConnection -> IO ()
insertDescriptors ps tc = do
  insertionResult <-
    runExceptT $ do
      unrelatedDK <- getUnrelatedDescriptorKey tc
      desKeys <- lift $ mapM insertDescriptorAndGetKey $ ps
      lift $ createBulkInfraRelations tc unrelatedDK desKeys
  either (T.IO.hPutStrLn stderr) pure insertionResult
 where
  insertDescriptorAndGetKey :: T.Text -> IO RecordKey
  insertDescriptorAndGetKey desName = do
    execute tc [r||] [desName]
    lastInsertRowId tc

{- |
 Create a new 'Descriptor` relation.

 Any Infra relations that the second given 'RecordKey` is a part of should be replaced
 automatically by SQLite.
-}
insertDescriptorRelation :: TaggedConnection -> RecordKey -> RecordKey -> IO ()
insertDescriptorRelation tc newMeta newInfra =
  execute tc q (newMeta, newInfra)
 where
  q =
    [r|
    INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
      VALUES (?,?)
    |]

{- |
 Convenience function for relating many 'Descriptor`s to one meta 'Descriptor`
-}
createBulkInfraRelations :: TaggedConnection -> RecordKey -> [RecordKey] -> IO ()
createBulkInfraRelations tc metaD =
  executeMany
    tc
    [r|INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
      VALUES (?,?)
    |]
    . zip (repeat metaD)

getUnrelatedDescriptorKey :: TaggedConnection -> ExceptT T.Text IO RecordKey
getUnrelatedDescriptorKey tc = do
  result <-
    lift $
      query_
        tc
        q ::
      ExceptT T.Text IO [Only RecordKey]
  maybe
    ( throwE
        "#UNRELATED# Descriptor not found.\n\
        \Some database operations may be impossible to perform.\n\
        \Please reinitialize database to generate #UNRELATED# Descriptor."
    )
    ( return . (\(Only x) -> x)
    )
    . head'
    $ result
 where
  q =
    [r|
    SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#'
    |]
