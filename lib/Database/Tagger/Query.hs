{-# LANGUAGE BangPatterns #-}
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
  allFiles,
  queryForUntaggedFiles,

  -- *** On 'File`

  -- | Queries that search based on some attribute of a 'File` type.
  queryForFileByPattern,
  queryForSingleFileByFileId,

  -- *** On 'Descriptor`

  -- | Queries that search based on some attribute of a 'Descriptor` type.
  flatQueryForFileByTagDescriptor,
  queryForFileBySubTagRelation,

  -- ** 'Descriptor` Queries

  -- | Queries that return 'Descriptor`s
  allDescriptors,
  queryForDescriptorByPattern,
  queryForSingleDescriptorByDescriptorId,
  getInfraChildren,
  getMetaParent,
  hasInfraRelations,

  -- ** 'Tag` Queries

  -- | Queries that return 'Tag`s. Probably won't be used very much.
  allTags,

  -- ** MetaDescriptor Queries

  -- | Queries on the MetaDescriptor table. Used in testing.
  allMetaDescriptorRows,

  -- * Operations
  -- $Operations

  -- ** 'File` Operations
  insertFiles,
  deleteFiles,
  updateFilePaths,

  -- ** 'Descriptor` Operations
  insertDescriptors,
  deleteDescriptors,
  updateDescriptors,

  -- ** 'Relation` Operations
  insertDescriptorRelation,

  -- ** 'Tag` Operations
  insertTags,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger.Connection
import Database.Tagger.Type
import System.IO (stderr)
import Tagger.Util
import Text.RawString.QQ (r)

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
 Query for a single 'File` with its id.
-}
queryForSingleFileByFileId :: RecordKey File -> TaggedConnection -> MaybeT IO File
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
 Performs a case-insensitive search for 'File`s that are tagged with a 'Descriptor`
 matching the text pattern provided.

 A flat search, meaning that any 'Descriptor` that tags an image will be searched,
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
 Query for a given subtag relationship.

 This query is single-depth, it searches for subtag relations that have been explicitly
 created and not relations that have been implicitly created because of relation nesting.
-}
queryForFileBySubTagRelation ::
  -- | The id of the meta 'Descriptor`, corresponds to the Tag.subTagOfId column.
  RecordKey Descriptor ->
  -- | The id of the infra 'Descriptor`, corresponds to the Tag.id column.
  RecordKey Descriptor ->
  TaggedConnection ->
  IO (HashSet.HashSet File)
queryForFileBySubTagRelation superK subK tc =
  HashSet.fromList <$> query tc q (superK, subK)
 where
  q =
    [r|
    SELECT
      f.id
      ,f.filePath
    FROM
      Tag t
    JOIN
      Tag t1
      ON t.id = t1.subTagOfId
    JOIN File f
      ON t.fileId = f.id
    WHERE
      t.descriptorId = ?
      AND t1.descriptorId = ?
    |]

{- |
 Query for 'File`s without tags.
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

{- |
 Returns all 'File`s in the database.
-}
allFiles :: TaggedConnection -> IO (HashSet.HashSet File)
allFiles tc = HashSet.fromList <$> query_ tc q
 where
  q =
    [r|
    SELECT
      id
      ,filePath
    FROM File
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
 Given a tuple of a new file path and an existing 'File`'s primary key,
  update that 'File`'s filePath field.

 This should not be called without also calling the system's equivalent "mv" command.
  Doing so may cause some Tagger operations to fail.
-}
updateFilePaths :: [(FilePath, RecordKey File)] -> TaggedConnection -> IO ()
updateFilePaths updates tc =
  executeMany tc q updates
 where
  q =
    [r|
    UPDATE File SET filePath = ? WHERE id = ?
    |]

{- |
 Given a list of labels, create new 'Descriptor` rows in the database.

 The new 'Descriptor`s will automatically be related to \#UNRELATED\#.

 This should be the only way that new 'Descriptor`s are added to a Tagger database.
 Doing so manually is not really a good idea unless care is taken to also manually relate
 new 'Descriptor`s to \#UNRELATED\#.
-}
insertDescriptors :: [T.Text] -> TaggedConnection -> IO ()
insertDescriptors ps tc = do
  insertionResult <-
    runExceptT $ do
      unrelatedDK <- getUnrelatedDescriptorKey tc
      desKeys <- lift $ mapM insertDescriptorAndGetKey ps
      lift $ createBulkInfraRelations tc unrelatedDK desKeys
  either (T.IO.hPutStrLn stderr) pure insertionResult
 where
  insertDescriptorAndGetKey :: T.Text -> IO (RecordKey Descriptor)
  insertDescriptorAndGetKey desName = do
    execute tc q [desName]
    lastInsertRowId tc
   where
    q =
      [r|
      INSERT INTO Descriptor (descriptor) VALUES (?)
      |]

{- |
 Delete a list of descriptor name matches from the database.
-}
deleteDescriptors :: [T.Text] -> TaggedConnection -> IO ()
deleteDescriptors ps tc =
  executeMany tc q (Only <$> ps)
 where
  q =
    [r|
    DELETE FROM Descriptor WHERE descriptor = ?
    |]

{- |
 Given a tuple of 'Text` and a 'Descriptor`'s primary key, relabel that 'Descriptor`.
-}
updateDescriptors :: [(T.Text, RecordKey File)] -> TaggedConnection -> IO ()
updateDescriptors updates tc =
  executeMany tc q updates
 where
  q =
    [r|
    UPDATE Descriptor SET descriptor = ? WHERE id = ?
    |]

{- |
 Returns all 'Descriptor`s in the database.
-}
allDescriptors :: TaggedConnection -> IO (HashSet.HashSet Descriptor)
allDescriptors tc = HashSet.fromList <$> query_ tc q
 where
  q =
    [r|
    SELECT
      id
      ,descriptor
    FROM
      Descriptor
    |]

{- |
 Query for 'Descriptor`s with a SQL pattern on the 'Descriptor`s' labels.
-}
queryForDescriptorByPattern ::
  T.Text -> TaggedConnection -> IO (HashSet.HashSet Descriptor)
queryForDescriptorByPattern p tc =
  HashSet.fromList <$> query tc q [p]
 where
  q =
    [r|
    SELECT
      id
      ,descriptor
    FROM 
      Descriptor
    WHERE 
      descriptor LIKE ?
    |]

{- |
 Query for a descriptor given an ID.
-}
queryForSingleDescriptorByDescriptorId ::
  RecordKey Descriptor ->
  TaggedConnection ->
  MaybeT IO Descriptor
queryForSingleDescriptorByDescriptorId dk tc = do
  result <- lift $ query tc q [dk] :: MaybeT IO [Descriptor]
  hoistMaybe . head' $ result
 where
  q =
    [r|
    SELECT
      id
      ,descriptor
    FROM
      Descriptor
    WHERE
      id = ?
    |]

{- |
 Retrieve a single layer relation of 'Descriptor`s that are Infra related to the
 'Descriptor` given as the id.
-}
getInfraChildren ::
  RecordKey Descriptor ->
  TaggedConnection ->
  IO (HashSet.HashSet Descriptor)
getInfraChildren dk tc = HashSet.fromList <$> query tc q [dk]
 where
  q =
    [r|
    SELECT
      d.id
      ,d.descriptor
    FROM MetaDescriptor md
    JOIN Descriptor d
      ON md.infraDescriptorId = d.id
    WHERE
      md.metaDescriptorId = ?
    |]

{- |
 Retrieve the 'Descriptor` that is Meta related to the given 'Descriptor` if it exists.

 Note that the actual implementation of this function only retrieves the first parent
 returned by the query. Multiple parents should never happen to a UNIQUE constraint
 in a Tagger's database.
-}
getMetaParent :: RecordKey Descriptor -> TaggedConnection -> MaybeT IO Descriptor
getMetaParent rk tc = do
  result <- lift $ query tc q [rk] :: MaybeT IO [Descriptor]
  hoistMaybe . head' $ result
 where
  q =
    [r|
    SELECT
      d.id
      ,d.descriptor
    FROM
      MetaDescriptor md
    JOIN
      Descriptor d
    ON
      md.metaDescriptorId = d.id
    WHERE
      md.infraDescriptorId = ?
    |]

{- |
 Return 'True` if the given 'Descriptor` is Meta related to anything.
-}
hasInfraRelations :: RecordKey Descriptor -> TaggedConnection -> IO Bool
hasInfraRelations dk tc = do
  result <- query tc q [dk] :: IO [Only Int]
  return . any ((==) 1 . (\(Only n) -> n)) $ result
 where
  q =
    [r|
    SELECT EXISTS
      (
        SELECT * 
        FROM MetaDescriptor 
        WHERE metaDescriptorId = ?
      )
    |]

{- |
 Returns all 'Tag`s in the database.

 Will likely be a pretty large set for mature databases.
-}
allTags :: TaggedConnection -> IO (HashSet.HashSet Tag)
allTags tc = HashSet.fromList <$> query_ tc q
 where
  q =
    [r|
    SELECT
      id
      ,fileId
      ,descriptorId
      ,subTagOfId
    FROM
      Tag
    |]

{- |
 Retrieve all rows in the MetaDescriptor table encoded as a HashSet of tuples of
 'Descriptor` ID's.
-}
allMetaDescriptorRows ::
  TaggedConnection -> IO (HashSet.HashSet (RecordKey Descriptor, RecordKey Descriptor))
allMetaDescriptorRows tc = HashSet.fromList <$> query_ tc q
 where
  q =
    [r|
    SELECT
      metaDescriptorId
      ,infraDescriptorId
    FROM 
      MetaDescriptor
    |]

{- |
 Create a new 'Descriptor` relation.

 Any Infra relations that the second given 'RecordKey` is a part of should be replaced
 automatically by SQLite.
-}
insertDescriptorRelation ::
  TaggedConnection -> RecordKey Descriptor -> RecordKey Descriptor -> IO ()
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
createBulkInfraRelations ::
  TaggedConnection ->
  RecordKey Descriptor ->
  [RecordKey Descriptor] ->
  IO ()
createBulkInfraRelations tc metaD =
  executeMany
    tc
    [r|INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
      VALUES (?,?)
    |]
    . zip (repeat metaD)

getUnrelatedDescriptorKey :: TaggedConnection -> ExceptT T.Text IO (RecordKey Descriptor)
getUnrelatedDescriptorKey tc = do
  result <-
    lift $
      query_
        tc
        q ::
      ExceptT T.Text IO [Only (RecordKey Descriptor)]
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

{- |
 Given a list of tag triples,
  Insert them as 'Tag`s into the database.
-}
insertTags ::
  [(RecordKey File, RecordKey Descriptor, Maybe (RecordKey Tag))] ->
  TaggedConnection ->
  IO ()
insertTags inserts tc =
  executeMany tc q inserts
 where
  q =
    [r|
    INSERT INTO Tag (fileId, descriptorId, subTagOfId)
      VALUES (?,?,?)
    |]