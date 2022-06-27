{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

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
  flatQueryForFileByTagDescriptorPattern,
  flatQueryForFileOnMetaRelation,
  flatQueryForFileOnMetaRelationPattern,

  -- *** Relational queries
  -- $Relational
  queryForFileByFilePatternAndFilePattern,
  queryForFileByFilePatternAndUntagged,
  queryForFileByDescriptorSubTagDescriptor,
  queryForFileByDescriptorSubTagMetaDescriptor,
  queryForFileByFilePatternAndDescriptor,
  queryForFileByFilePatternAndMetaDescriptor,

  -- ** 'TaggedFile` and 'ConcreteTaggedFile` Queries

  -- | Fairly costly compared to most other queries, so should be used sparingly.
  queryForTaggedFileWithFileId,
  queryForConcreteTaggedFileWithFileId,

  -- ** 'Descriptor` Queries

  -- | Queries that return 'Descriptor`s
  allDescriptors,

  -- *** On 'Descriptor`
  queryForDescriptorByPattern,
  queryForSingleDescriptorByDescriptorId,
  getInfraChildren,
  getAllInfra,
  getMetaParent,

  -- *** On 'Tag`
  queryForSingleDescriptorByTagId,

  -- ** 'Tag` Queries

  -- | Queries that return 'Tag`s.
  allTags,

  -- *** On 'File`
  queryForFileTagsByFileId,

  -- ** MetaDescriptor Queries

  -- | Queries on the MetaDescriptor table. Used in testing.
  allMetaDescriptorRows,

  -- ** Misc.
  hasInfraRelations,

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

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.HashSet as HashSet
import Data.HierarchyMap
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger.Connection
import Database.Tagger.Type
import System.IO (stderr)
import Tagger.Util
import Text.RawString.QQ (r)

{- $Relational
 Queries hand-written to operate more efficiently on relations between two
 'QueryCriteria` and two given text patterns.

 These are primarily used by the TaggerQL engine to place as much work as possible
 in sqlite rather than intersecting large sets of data in Haskell.
-}

{- |
 Performs a case-insensitive search of all registered file paths, tagged or not.

 Can use SQL wildcards like % or _
-}
queryForFileByPattern :: T.Text -> TaggedConnection -> IO [File]
queryForFileByPattern p tc = query tc q [p]
 where
  q =
    [r|
      SELECT
        id
        ,filePath
      FROM
        File
      WHERE
        filePath LIKE ? ESCAPE '\'
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
 Given two file patterns, find 'File`s whose paths match both.
-}
queryForFileByFilePatternAndFilePattern ::
  T.Text ->
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByFilePatternAndFilePattern fpx fpy tc =
  query tc q (fpx, fpy)
 where
  q =
    [r|
    SELECT
      id
      ,filePath
    FROM File
    WHERE filePath LIKE ? ESCAPE '\'
      AND filePath LIKE ? ESCAPE '\'
    |]

{- |
 Query for 'File`s whose paths match the given pattern and have no 'Tag`s.
-}
queryForFileByFilePatternAndUntagged ::
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByFilePatternAndUntagged p tc =
  query tc q [p]
 where
  q =
    [r|
    SELECT
      f.id
      ,f.filePath
    FROM File f
    LEFT JOIN Tag t
      ON f.id = t.fileId
    WHERE f.filePath LIKE ? ESCAPE '\'
      AND t.fileId IS NULL
    |]

{- |
 A flat search, meaning that any 'Descriptor` that tags an image will be searched,
 regardless of whether or not it is a subtag or not.
-}
flatQueryForFileByTagDescriptor :: RecordKey Descriptor -> TaggedConnection -> IO [File]
flatQueryForFileByTagDescriptor p tc = query tc q [p]
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
        d.id = ?
      |]

{- |
 Flat query for 'File`s that are tagged with the given 'Descriptor` and all 'Descriptor`s
 that are infra to it.
-}
flatQueryForFileOnMetaRelation :: RecordKey Descriptor -> TaggedConnection -> IO [File]
flatQueryForFileOnMetaRelation dk tc = queryNamed tc q [":metaDes" := dk]
 where
  q =
    [r|
    WITH RECURSIVE rec (infraDescriptorId) AS (
      SELECT :metaDes
      UNION ALL
      SELECT md.infraDescriptorId
      FROM rec r
      JOIN MetaDescriptor md
        ON r.infraDescriptorId = md.metaDescriptorId
    )
    SELECT
      f.id
      ,f.filePath
    FROM rec r
    JOIN Tag t
      ON r.infraDescriptorId = t.descriptorId
    JOIN File f
      ON t.fileId = f.id
    |]

{- |
 Like 'flatQueryForFileOnMetaRelation` but given a text pattern instead of 'RecordKey`

 The text pattern can correspond to more than one 'Descriptor` and all infra
 children of each 'Descriptor` will be returned.
-}
flatQueryForFileOnMetaRelationPattern :: T.Text -> TaggedConnection -> IO [File]
flatQueryForFileOnMetaRelationPattern p tc = queryNamed tc q [":metaDesPattern" := p]
 where
  q =
    [r|
    WITH RECURSIVE rec(infraDescriptorId) AS (
      SELECT id
      FROM Descriptor
      WHERE descriptor LIKE :metaDesPattern ESCAPE '\'
      UNION ALL
      SELECT md.infraDescriptorId
      FROM rec r
      JOIN MetaDescriptor md
        ON r.infraDescriptorId = md.metaDescriptorId
    )
    SELECT
      f.id
      ,f.filePath
    FROM rec r
    JOIN Tag t
      ON r.infraDescriptorId = t.descriptorId
    JOIN File f
      ON t.fileId = f.id
    |]

{- |
 Given two 'Descriptor` patterns, query for 'File`s that are tagged with the
 first and subtagged with the second.
-}
queryForFileByDescriptorSubTagDescriptor ::
  T.Text ->
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByDescriptorSubTagDescriptor dp stdp tc =
  queryNamed tc q [":desPattern" := dp, ":subDesPattern" := stdp]
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
      t.descriptorId LIKE :desPattern ESCAPE '\'
      AND t1.descriptorId LIKE :subDesPattern ESCAPE '\'
    |]

{- |
 Given two 'Descriptor` patterns, query for 'File`s that are tagged with
 the first and subtagged by any 'Descriptor` that is infra to the second, inclusive.
-}
queryForFileByDescriptorSubTagMetaDescriptor ::
  T.Text ->
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByDescriptorSubTagMetaDescriptor dp stdp tc =
  queryNamed tc q [":desPattern" := dp, ":subDesPattern" := stdp]
 where
  q =
    [r|
    WITH RECURSIVE rec_des(id) AS (
      SELECT id
      FROM Descriptor
      WHERE descriptor LIKE :subDesPattern ESCAPE '\'
      UNION
      SELECT md.infraDescriptorId
      FROM rec_des rd
      JOIN MetaDescriptor md
        ON rd.id = md.metaDescriptorId
    )
    SELECT
      f.id
      ,f.filePath
    FROM Tag st
    JOIN File f
      ON st.fileId = f.id
    JOIN Tag it
      ON st.id = it.subTagOfId
    JOIN Descriptor d
      ON st.descriptorId = d.id
    WHERE d.descriptor LIKE :desPattern ESCAPE '\'
      AND it.descriptorId IN rec_des
    |]

{- |
 Return a set of 'File`s that have the first given pattern in their file paths
 and are tagged with 'Descriptor`s like the second given pattern.

 Synonymous with intersecting a file pattern query and a flat descriptor query.
-}
queryForFileByFilePatternAndDescriptor ::
  T.Text ->
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByFilePatternAndDescriptor fp dp tc =
  queryNamed tc q [":filePattern" := fp, ":desPattern" := dp]
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
    WHERE f.filePath LIKE :filePattern ESCAPE '\'
      AND d.descriptor LIKE :desPattern ESCAPE '\'
    |]

{- |
 Return a set of 'File`s that match the first given pattern and are tagged by
 any 'Descriptor` that is infra from 'Descriptor`s that match the second given pattern.
-}
queryForFileByFilePatternAndMetaDescriptor ::
  T.Text ->
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByFilePatternAndMetaDescriptor fp dp tc =
  queryNamed tc q [":filePattern" := fp, ":desPattern" := dp]
 where
  q =
    [r|
    WITH RECURSIVE rec_tree(id) AS (
      SELECT id
      FROM Descriptor
      WHERE descriptor LIKE :desPattern ESCAPE '\'
      UNION
      SELECT md.infraDescriptorId
      FROM rec_tree rt
      JOIN MetaDescriptor md
        ON rt.id = md.metaDescriptorId
    )
    SELECT
      f.id
      ,f.filePath
    FROM rec_tree rt
    JOIN Tag t
      ON rt.id = t.descriptorId
    JOIN File f
      ON t.fileId = f.id
    WHERE f.filePath LIKE :filePattern ESCAPE '\'
    |]

{- |
 Return a set of all 'File`s that have a 'Tag` where the 'Descriptor` matches
 the given pattern.

 Previous implementations did something like this:

 @
  \\p -> do $
    ds <- queryForDescriptorByPattern p
    fmap concat . mapM (flatQueryForFileByTagDescriptor . descriptorId) $ ds
 @

 This query is more efficient and handles all of that in one SQL query, but may return
   some duplicates.
-}
flatQueryForFileByTagDescriptorPattern :: T.Text -> TaggedConnection -> IO [File]
flatQueryForFileByTagDescriptorPattern p tc = query tc q [p]
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
    WHERE d.descriptor LIKE ? ESCAPE '\'
    |]

{- |
 Given a 'File` ID, return the corresponding 'TaggedFile` if it exists.
-}
queryForTaggedFileWithFileId :: RecordKey File -> TaggedConnection -> MaybeT IO TaggedFile
queryForTaggedFileWithFileId rk tc = do
  guard =<< lift (doesFileExist rk tc)
  fileTags <- fmap HashSet.fromList . lift $ queryForFileTagsByFileId rk tc
  return $ TaggedFile rk fileTags

{- |
 Given a 'File` ID, return the corresponding 'ConcreteTaggedFile` if it exists.

 Intended to be called for only one file at a time. Calling it for large numbers of
  'File`s is quite costly and provides no real benefit in the Tagger GUI front-end.
-}
queryForConcreteTaggedFileWithFileId ::
  RecordKey File ->
  TaggedConnection ->
  MaybeT IO ConcreteTaggedFile
queryForConcreteTaggedFileWithFileId rk tc = do
  file <- queryForSingleFileByFileId rk tc
  fileTags <- lift $ queryForFileTagsByFileId rk tc
  concreteFileTags <-
    lift
      . fmap (map (second HashSet.fromList) . catMaybes)
      . mapM (runMaybeT . traverseRelationTuple . getRelationTuple)
      $ fileTags
  let fileTagMap = inserts concreteFileTags empty
  return $ ConcreteTaggedFile file fileTagMap
 where
  getRelationTuple (Tag t _ _ ms) =
    maybe (t, []) (,[t]) ms
  traverseRelationTuple (x, xs) = do
    md <- queryForSingleDescriptorByTagId x tc
    ids <-
      lift
        . catMaybeTM (`queryForSingleDescriptorByTagId` tc)
        $ xs
    return (md, ids)
  second f (x, y) = (x, f y)

{- |
 Query for 'File`s without tags.
-}
queryForUntaggedFiles :: TaggedConnection -> IO [File]
queryForUntaggedFiles tc = query_ tc q
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
allFiles :: TaggedConnection -> IO [File]
allFiles tc = query_ tc q
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
allDescriptors :: TaggedConnection -> IO [Descriptor]
allDescriptors tc = query_ tc q
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
  T.Text -> TaggedConnection -> IO [Descriptor]
queryForDescriptorByPattern p tc =
  query tc q [p]
 where
  q =
    [r|
    SELECT
      id
      ,descriptor
    FROM 
      Descriptor
    WHERE 
      descriptor LIKE ? ESCAPE '\'
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
 Given a 'Tag` ID, return the 'Descriptor` used in that 'Tag` if it exists.
-}
queryForSingleDescriptorByTagId ::
  RecordKey Tag -> TaggedConnection -> MaybeT IO Descriptor
queryForSingleDescriptorByTagId rk tc = do
  result <- lift $ query tc q [rk] :: MaybeT IO [Descriptor]
  hoistMaybe . head' $ result
 where
  q =
    [r|
    SELECT
      d.id
      ,d.descriptor
    FROM 
      Tag t
    JOIN
      Descriptor d
    ON
      t.descriptorId = d.id
    WHERE
      t.id = ?
    |]

{- |
 Retrieve a single layer relation of 'Descriptor`s that are Infra related to the
 'Descriptor` given as the id.

 Will always return a subset of 'getAllInfra`.
-}
getInfraChildren ::
  RecordKey Descriptor ->
  TaggedConnection ->
  IO [Descriptor]
getInfraChildren dk tc = query tc q [dk]
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
 Given a 'Descriptor` ID, return a set of all 'Descriptor` Descriptors.

 Includes the given 'Descriptor`.
-}
getAllInfra ::
  RecordKey Descriptor ->
  TaggedConnection ->
  IO [Descriptor]
getAllInfra rk tc =
  queryNamed tc q [":metaDes" := rk]
 where
  q =
    [r|
    WITH RECURSIVE rec(infraDescriptorId) AS (
      SELECT :metaDes
      UNION ALL
      SELECT md.infraDescriptorId
      FROM rec r
      JOIN MetaDescriptor md
        ON r.infraDescriptorId = md.metaDescriptorId
    )
    SELECT
      d.id
      ,d.descriptor
    FROM rec r
    JOIN Descriptor d
      ON r.infraDescriptorId = d.id
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
allTags :: TaggedConnection -> IO [Tag]
allTags tc = query_ tc q
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
 Returns all 'Tag`s for a given 'File`.
-}
queryForFileTagsByFileId :: RecordKey File -> TaggedConnection -> IO [Tag]
queryForFileTagsByFileId rk tc =
  query tc q [rk]
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
    WHERE
      fileId = ?
    |]

{- |
 Retrieve all rows in the MetaDescriptor table encoded as a list of tuples of
 'Descriptor` ID's.
-}
allMetaDescriptorRows ::
  TaggedConnection -> IO [(RecordKey Descriptor, RecordKey Descriptor)]
allMetaDescriptorRows tc = query_ tc q
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
  RecordKey Descriptor -> RecordKey Descriptor -> TaggedConnection -> IO ()
insertDescriptorRelation newMeta newInfra tc =
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
insertTags toInsert tc =
  executeMany tc q toInsert
 where
  q =
    [r|
    INSERT INTO Tag (fileId, descriptorId, subTagOfId)
      VALUES (?,?,?)
    |]

{- |
 Given a 'File` ID, show if it exists in the database.
-}
doesFileExist :: RecordKey File -> TaggedConnection -> IO Bool
doesFileExist rk tc = do
  result <- query tc q [rk] :: IO [Only Int]
  return . any ((==) 1 . (\(Only n) -> n)) $ result
 where
  q =
    [r|
    SELECT
      EXISTS
        (
          SELECT
            *
          FROM
            File
          WHERE
            id = ?
        )
    |]