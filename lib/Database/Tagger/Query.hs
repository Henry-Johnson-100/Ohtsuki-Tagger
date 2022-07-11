{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}

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
  queryForFileByMetaDescriptorSubTagDescriptor,
  queryForFileByMetaDescriptorSubTagMetaDescriptor,

  -- ** 'TaggedFile` and 'ConcreteTaggedFile` Queries

  -- | Fairly costly compared to most other queries, so should be used sparingly.
  queryForTaggedFileWithFileId,
  queryForConcreteTaggedFileWithFileId,

  -- ** 'Descriptor` Queries

  -- | Queries that return 'Descriptor`s
  allDescriptors,
  getUnrelatedDescriptor,

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
  queryForTagByFileAndDescriptorKey,
  queryForTagByFileAndDescriptorKeyAndNullSubTagOf,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagBySubTagTriple,

  -- *** On 'File`
  queryForFileTagsByFileId,

  -- ** MetaDescriptor Queries

  -- | Queries on the MetaDescriptor table. Used in testing.
  allMetaDescriptorRows,

  -- ** Misc.
  hasInfraRelations,
  getTagOccurrencesByDescriptorKeys,
  getTagOccurrencesByFileKey,
  getLastAccessed,
  getLastSaved,

  -- * Operations
  -- $Operations

  -- ** 'File` Operations
  insertFiles,
  deleteFiles,
  updateFilePaths,

  -- ** 'Descriptor` Operations
  insertDescriptors,
  deleteDescriptors,
  deleteDescriptors',
  updateDescriptors,
  updateDescriptors',

  -- ** 'Relation` Operations
  insertDescriptorRelation,

  -- ** 'Tag` Operations
  deleteTags,
  unSubTags,
  moveSubTags,

  -- *** 'Tag` Insertion
  -- $FailedInsertion
  insertTags,
  unsafeInsertTag,
) where

import Control.Monad (guard, join)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import qualified Data.HierarchyMap as HAM
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.OccurrenceHashMap.Internal (OccurrenceHashMap)
import qualified Data.OccurrenceHashMap.Internal as OHM (
  fromList,
  unions,
 )
import qualified Data.OccurrenceMap.Internal as OM
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Connection (
  NamedParam ((:=)),
  Only (Only),
  execute,
  executeMany,
  executeNamed,
  execute_,
  lastInsertRowId,
  query,
  queryNamed,
  query_,
 )
import Database.Tagger.Query.Type (TaggerQuery)
import Database.Tagger.Type (
  ConcreteTag (ConcreteTag),
  ConcreteTaggedFile (ConcreteTaggedFile),
  Descriptor (Descriptor, descriptor, descriptorId),
  File,
  RecordKey,
  Tag (Tag, tagSubtagOfId),
  TaggedConnection,
  TaggedFile (TaggedFile),
 )
import Tagger.Util (catMaybeTM, head', hoistMaybe)
import Text.RawString.QQ (r)

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

{- $Relational
 Queries hand-written to operate more efficiently on relations between two
 'QueryCriteria` and two given text patterns.

 These are primarily used by the TaggerQL engine to place as much work as possible
 in sqlite rather than intersecting large sets of data in Haskell.
-}

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
 Given two patterns to match on 'Descriptor`s, find all files that
 are tagged with any 'Descriptor` infra to the first and subtagged by any
 'Descriptor` matched by the second.
-}
queryForFileByMetaDescriptorSubTagDescriptor ::
  T.Text ->
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByMetaDescriptorSubTagDescriptor mdp dp tc =
  queryNamed tc q [":metaDesPattern" := mdp, ":subDesPattern" := dp]
 where
  q =
    [r|
    WITH joined_tags(id,fileId,descriptorId,subTagOfId) AS (
      WITH super_tags(id, fileId, descriptorId, subTagOfId) AS (
        WITH RECURSIVE super_meta_descriptors(id) AS (
          SELECT id
          FROM Descriptor
          WHERE descriptor LIKE :metaDesPattern ESCAPE '\'
          UNION
          SELECT md.infraDescriptorId
          FROM super_meta_descriptors smd
          JOIN MetaDescriptor md
            ON smd.id = md.metaDescriptorId
        )
        SELECT
          t.id
          ,t.fileId
          ,t.descriptorId
          ,t.subTagOfId
        FROM Tag t
        JOIN super_meta_descriptors smd
          ON t.descriptorId = smd.id
      ),
      sub_tags(id,fileId,descriptorId,subTagOfId) AS (
        WITH sub_descriptors(id) AS (
          SELECT id
          FROM Descriptor
          WHERE descriptor LIKE :subDesPattern ESCAPE '\'
        )
        SELECT
          t.id
          ,t.fileId
          ,t.descriptorId
          ,t.subTagOfId
        FROM Tag t
        JOIN sub_descriptors sd
          ON t.descriptorId = sd.id
      )
      SELECT   
        sup.id
        ,sup.fileId
        ,sup.descriptorId
        ,sup.subTagOfId
      FROM super_tags sup
      JOIN sub_tags st
        ON sup.id = st.subTagOfId
    )
    SELECT
      f.id
      ,f.filePath
    FROM joined_tags jt
    JOIN File f
      ON jt.fileId = f.id
    |]

{- |
 Given two patterns to match on 'Descriptor`s, find all files that
 are tagged with any 'Descriptor` infra to the first and subtagged by any
 'Descriptor` infra to the one matched by the second.
-}
queryForFileByMetaDescriptorSubTagMetaDescriptor ::
  T.Text ->
  T.Text ->
  TaggedConnection ->
  IO [File]
queryForFileByMetaDescriptorSubTagMetaDescriptor bp sp tc =
  queryNamed tc q [":metaDesPattern" := bp, ":subMetaDesPattern" := sp]
 where
  q =
    [r|
    WITH joined_tags(id, fileId, descriptorId, subTagOfId) AS (
      WITH super_tags(id,fileId,descriptorId,subTagOfId) AS (
        WITH RECURSIVE super_meta_descriptors(id) AS (
          SELECT id
          FROM Descriptor
          WHERE descriptor LIKE :metaDesPattern ESCAPE '\'
          UNION
          SELECT md.infraDescriptorId
          FROM super_meta_descriptors smd
          JOIN MetaDescriptor md
            ON smd.id = md.metaDescriptorId
        )
        SELECT
          t.id
          ,t.fileId
          ,t.descriptorId
          ,t.subTagOfId
        FROM Tag t
        JOIN super_meta_descriptors smd
          ON t.descriptorId = smd.id
      ),
      sub_tags(id,fileId,descriptorId,subTagOfId) AS (
        WITH RECURSIVE sub_meta_descriptors(id) AS (
          SELECT id
          FROM Descriptor
          WHERE descriptor LIKE :subMetaDesPattern
          UNION
          SELECT md.infraDescriptorId
          FROM sub_meta_descriptors smd
          JOIN MetaDescriptor md
            ON smd.id = md.metaDescriptorId
        )
        SELECT
          t.id
          ,t.fileId
          ,t.descriptorId
          ,t.subTagOfId
        FROM Tag t
        JOIN sub_meta_descriptors smd
          ON t.descriptorId = smd.id
      )
    SELECT
      sup.id
      ,sup.fileId
      ,sup.descriptorId
      ,sup.subTagOfId
    FROM super_tags sup
    JOIN sub_tags st
      ON sup.id = st.subTagOfId
    )
    SELECT
      f.id
      ,f.filePath
    FROM joined_tags jt
    JOIN File f
      ON jt.fileId = f.id
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
  let tagRelationTuples = getRelationTuple <$> fileTags
  concreteTagRelationTuples <-
    map (second HashSet.fromList)
      <$> mapM traverseRelationTuple tagRelationTuples
  return $ ConcreteTaggedFile file (HAM.fromList concreteTagRelationTuples)
 where
  getRelationTuple (Tag t _ _ ms) =
    maybe (t, []) (,[t]) ms
  traverseRelationTuple (x, xs) = do
    md <- derefTag x
    ids <-
      lift
        . catMaybeTM derefTag
        $ xs
    return (md, ids)
  second f (x, y) = (x, f y)
  derefTag :: RecordKey Tag -> MaybeT IO ConcreteTag
  derefTag tid = do
    results <- lift $ query tc q [tid]
    let result = head' results
    hoistMaybe (toConcreteTag <$> result)
   where
    toConcreteTag (tid', did, dp, mstid) = ConcreteTag tid' (Descriptor did dp) mstid
    q =
      [r|
      SELECT
        t.id
        ,d.id
        ,d.descriptor
        ,t.subTagOfId
      FROM Tag t
      JOIN Descriptor d
        ON t.descriptorId = d.id
      WHERE t.id = ?
      |]

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
deleteFiles :: [RecordKey File] -> TaggedConnection -> IO ()
deleteFiles ps tc = executeMany tc q (Only <$> ps)
 where
  q =
    [r|
    DELETE FROM File WHERE id = ?
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
  executeMany tc q (Only <$> ps)
  execute_ tc unrelateUnrelatedTaggerQuery
 where
  q =
    [r|
    INSERT INTO Descriptor (descriptor) VALUES (?)
    |]

{- |
 Delete a list of 'Descriptor`s from the database.
 Does not delete any immutable 'Descriptor`s, I.E. 'Descriptor`s infixed by '#'
 Use 'deleteDescriptors'' if this is desired.

 Also runs a maintenance execution on the db, inserting all 'Descriptor`s that are not
 infra to anything as infra to \#UNRELATED\#.
-}
deleteDescriptors :: [RecordKey Descriptor] -> TaggedConnection -> IO ()
deleteDescriptors dks tc = do
  corrDks <-
    catMaybes
      <$> mapM (runMaybeT . flip queryForSingleDescriptorByDescriptorId tc) dks
  let mutDs =
        filter
          ( \(descriptor -> dp) ->
              not
                ( "#" `T.isPrefixOf` dp
                    && "#" `T.isSuffixOf` dp
                )
          )
          corrDks
  deleteDescriptors' (descriptorId <$> mutDs) tc

{- |
 Delete a list of 'Descriptor`s from the database.

 Also runs a maintenance execution on the db, inserting all 'Descriptor`s that are not
 infra to anything as infra to \#UNRELATED\#.
-}
deleteDescriptors' :: [RecordKey Descriptor] -> TaggedConnection -> IO ()
deleteDescriptors' ps tc = do
  executeMany tc q (Only <$> ps)
  execute_ tc unrelateUnrelatedTaggerQuery
 where
  q =
    [r|
    DELETE FROM Descriptor WHERE id = ?
    |]

unrelateUnrelatedTaggerQuery :: TaggerQuery
unrelateUnrelatedTaggerQuery =
  [r|
    INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
      SELECT
        (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#')
        ,id
      FROM Descriptor
      WHERE id NOT IN (SELECT infraDescriptorId FROM MetaDescriptor)
        AND id <> (SELECT id FROM Descriptor WHERE descriptor = '#ALL#')
    |]

{- |
 Given a tuple of 'Text` and a 'Descriptor`'s primary key, relabel that 'Descriptor`.

 Does not update immutable 'Descriptor`s, I.E. infixed by '#'.
 Use 'updateDescriptors'' if this is desired.
-}
updateDescriptors :: [(T.Text, RecordKey Descriptor)] -> TaggedConnection -> IO ()
updateDescriptors updates tc = do
  corrDkTuples <-
    catMaybes
      <$> mapM
        (runMaybeT . secondM (flip queryForSingleDescriptorByDescriptorId tc))
        updates
  let mutDTuples =
        second descriptorId
          <$> filter
            ( \(descriptor . snd -> dp) ->
                not
                  ( "#" `T.isPrefixOf` dp
                      && "#" `T.isSuffixOf` dp
                  )
            )
            corrDkTuples
  updateDescriptors' mutDTuples tc
 where
  second f (x, y) = (x, f y)
  secondM fm (x, y) = do
    y' <- fm y
    return (x, y')

{- |
 Given a tuple of 'Text` and a 'Descriptor`'s primary key, relabel that 'Descriptor`.
-}
updateDescriptors' :: [(T.Text, RecordKey Descriptor)] -> TaggedConnection -> IO ()
updateDescriptors' updates tc =
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
 Given a list of 'Descriptor`s, retrieve an 'OccurrenceMap` for how many distinct
 'File`s are tagged for each one.
-}
getTagOccurrencesByDescriptorKeys :: [RecordKey Descriptor] -> TaggedConnection -> IO OM.OccurrenceMap
getTagOccurrencesByDescriptorKeys ds tc = do
  ots <- mapM getOccurrenceTuple ds :: IO [(RecordKey Descriptor, Int)]
  return . OM.fromList $ ots
 where
  getOccurrenceTuple dk = do
    o <- fromMaybe 0 . head' . map (\(Only n) -> n) <$> query tc q [dk]
    return (dk, o)
   where
    q =
      [r|
        SELECT COUNT (DISTINCT fileId)
        FROM Tag
        WHERE descriptorId = ?
        |]

{- |
 Returns an 'OccurrenceHashMap` with occurrences of each 'Descriptor` that are
 taggond on the 'File`s provided.
-}
getTagOccurrencesByFileKey ::
  Traversable t =>
  t (RecordKey File) ->
  TaggedConnection ->
  IO (OccurrenceHashMap Descriptor)
getTagOccurrencesByFileKey fks tc = do
  results <-
    F.toList
      <$> mapM
        ( query tc q . Only :: RecordKey File -> IO [(RecordKey Descriptor, Text, Int)]
        )
        fks
  let constructFromTuple (dk, dp, o) = (Descriptor dk dp, o)
  return . OHM.unions $ OHM.fromList . map constructFromTuple <$> results
 where
  q =
    [r|
    SELECT
      d.id
      ,d.descriptor
      ,COUNT(*)
    FROM Tag t
    JOIN Descriptor d
      ON t.descriptorId = d.id
    WHERE fileId = ?
    GROUP BY descriptorId
    |]

{- |
 Retrieve the first timestamp from the column lastAccessed in the db info table.
-}
getLastAccessed :: TaggedConnection -> MaybeT IO Text
getLastAccessed tc = do
  result <- lift (map (\(Only x) -> x) <$> query_ tc q) :: MaybeT IO [Maybe Text]
  hoistMaybe . join . head' $ result
 where
  q =
    [r|
    SELECT lastAccessed
    FROM TaggerDBInfo
    |]

{- |
 Retrieve the first timestamp from the column lastBackup in the db info table.
-}
getLastSaved :: TaggedConnection -> MaybeT IO Text
getLastSaved tc = do
  result <- lift (map (\(Only x) -> x) <$> query_ tc q) :: MaybeT IO [Maybe Text]
  hoistMaybe . join . head' $ result
 where
  q =
    [r|
    SELECT lastBackup
    FROM TaggerDBInfo
    |]

{- |
 For the given Tag Triple, return 'True` if it will violate the UNIQUE constraint in
 the Tag table.
-}
tagUniqueConstraintExists ::
  (RecordKey File, RecordKey Descriptor, Maybe (RecordKey Tag)) ->
  TaggedConnection ->
  IO Bool
tagUniqueConstraintExists tagToCheck tc = do
  result <- map (\(Only n) -> n) <$> query tc q tagToCheck :: IO [Int]
  return $ elem 1 result
 where
  q =
    [r|
    SELECT EXISTS(
      SELECT *
      FROM Tag
      WHERE fileId = ?
        AND descriptorId = ?
        AND subTagOfId = ?
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
 Return a list of 'Tag`s for the given 'File` and 'Descriptor`
 pattern.
-}
queryForTagByFileAndDescriptorKey ::
  RecordKey File -> RecordKey Descriptor -> TaggedConnection -> IO [Tag]
queryForTagByFileAndDescriptorKey fk dk tc =
  queryNamed tc q [":fileKey" := fk, ":desKey" := dk]
 where
  q =
    [r|
    SELECT
      id
      ,fileId
      ,descriptorId
      ,subTagOfId
    FROM Tag
    WHERE fileId = :fileKey
      AND descriptorId = :desKey
    |]

{- |
 Like 'queryForTagByFileAndDescriptorKey` with the added constraint
 of requiring a null subTagOfId column.
-}
queryForTagByFileAndDescriptorKeyAndNullSubTagOf ::
  RecordKey File ->
  RecordKey Descriptor ->
  TaggedConnection ->
  IO [Tag]
queryForTagByFileAndDescriptorKeyAndNullSubTagOf fk dk tc = query tc q (fk, dk)
 where
  q =
    [r|
    SELECT
      id
      ,fileId
      ,descriptorId
      ,subTagOfId
    FROM Tag
    WHERE fileId = ?
      AND descriptorId = ?
      AND subTagOfId IS NULL
    |]

{- |
 Like 'queryForTagByFileAndDescriptorKey` with the added constraints
 of requiring a null subTagOfId column and searching on a 'Descriptor` pattern rather
 than key.
-}
queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf ::
  RecordKey File ->
  Text ->
  TaggedConnection ->
  IO [Tag]
queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf fk dk tc = query tc q (fk, dk)
 where
  q =
    [r|
    SELECT
      t.id
      ,t.fileId
      ,t.descriptorId
      ,t.subTagOfId
    FROM Tag t
    JOIN Descriptor d
      ON t.descriptorId = d.id
    WHERE t.fileId = ?
      AND d.descriptor LIKE ? ESCAPE '\'
      AND t.subTagOfId IS NULL
    |]

{- |
 Given a 'Tag` triple with a non-null subTagOfId,
 return the 'Tag`s that match.
-}
queryForTagBySubTagTriple ::
  (RecordKey File, RecordKey Descriptor, RecordKey Tag) ->
  TaggedConnection ->
  IO [Tag]
queryForTagBySubTagTriple triple tc = query tc q triple
 where
  q =
    [r|
    SELECT
      id
      ,fileId
      ,descriptorId
      ,subTagOfId
    FROM Tag
    WHERE fileId = ?
      AND descriptorId = ?
      AND subTagOfId = ?
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
 Given 'Tag` ID's, delete them from the database.
-}
deleteTags :: [RecordKey Tag] -> TaggedConnection -> IO ()
deleteTags tids tc =
  mapM_ (\t -> executeNamed tc q [":tagId" := t]) tids
 where
  q =
    [r|
    DELETE FROM Tag WHERE id = :tagId OR subTagOfId = :tagId
    |]

{- |
 For all given 'Tag`s, remove them as subtags and set as normal top-level tags.
-}
unSubTags :: [RecordKey Tag] -> TaggedConnection -> IO ()
unSubTags tids tc = executeMany tc q (Only <$> tids)
 where
  q =
    [r|
    UPDATE Tag SET subTagOfId = NULL WHERE id = ?
    |]

{- |
 Given a list of tuples consisting of 'Tag` keys, update all 'Tag`s that
 are subtags of the first to be subtags of the second.
-}
moveSubTags :: [(RecordKey Tag, RecordKey Tag)] -> TaggedConnection -> IO ()
moveSubTags tids tc =
  mapM_
    ( \(oldTK, newTK) ->
        executeNamed
          tc
          q
          [":oldSuperKey" := oldTK, ":newSuperKey" := newTK]
    )
    tids
 where
  q =
    [r|
    UPDATE Tag SET subTagOfId = :newSuperKey WHERE subTagOfId = :oldSuperKey
    |]

{- |
 Search for the \#UNRELATED\# 'Descriptor`, throw an exception if it is not found.
-}
getUnrelatedDescriptor :: TaggedConnection -> ExceptT T.Text IO Descriptor
getUnrelatedDescriptor tc = do
  result <-
    lift $
      query_
        tc
        q ::
      ExceptT T.Text IO [Descriptor]
  maybe
    ( throwE
        "#UNRELATED# Descriptor not found.\n\
        \Some database operations may be impossible to perform.\n\
        \Please reinitialize database to generate #UNRELATED# Descriptor."
    )
    return
    . head'
    $ result
 where
  q =
    [r|
    SELECT id, descriptor FROM Descriptor WHERE descriptor = '#UNRELATED#'
    |]

{- |
 Given a list of tag triples,
  Insert them as 'Tag`s into the database.

Some insertions may fail. Failed insertions are not included in the result set of
  of this function's 'RecordKey`s

If this behavior is not desired, see 'unsafeInsertTag`
-}
insertTags ::
  [(RecordKey File, RecordKey Descriptor, Maybe (RecordKey Tag))] ->
  TaggedConnection ->
  IO [RecordKey Tag]
insertTags toInsert tc = catMaybes <$> mapM (`insertTag` tc) toInsert

{- $FailedInsertion
 For each Tag Triple, if its third member is 'Nothing` and there exists at least one
  'Tag` in the database that matches that 'File` ID and 'Descriptor` ID whose subTagOfId
  column is NOT NULL then it is not inserted and an @IO Nothing@ is returned.

If the third member is not 'Nothing` then a precheck occurs if the given triple will
  violate the database's UNIQUE constraint. If it does, then @IO Nothing@ is returned.
-}

{- |
 Insert one 'Tag` and retrieve its ID.

 For each triple, if its third member is 'Nothing` and there exists at least one
  'Tag` in the database that matches that 'File` ID and 'Descriptor` ID whose subTagOfId
  column is NOT NULL then it is not inserted and an @IO Nothing@ is returned.

If the third member is not 'Nothing` then a precheck occurs if the given triple will
  violate the database's UNIQUE constraint. If it does, then @IO Nothing@ is returned.

Otherwise, an @IO (Just (RecordKey Tag))@ is returned, corresponding to the successful
insertion.
-}
insertTag ::
  (RecordKey File, RecordKey Descriptor, Maybe (RecordKey Tag)) ->
  TaggedConnection ->
  IO (Maybe (RecordKey Tag))
insertTag toInsert@(fk, dk, subTagOf) tc =
  case subTagOf of
    Nothing -> do
      topLevelTagExists <-
        any (isNothing . tagSubtagOfId)
          <$> queryForTagByFileAndDescriptorKey fk dk tc
      if topLevelTagExists
        then return Nothing
        else Just <$> unsafeInsertTag toInsert tc
    _ -> do
      willViolateUniqueConstraint <- tagUniqueConstraintExists toInsert tc
      if willViolateUniqueConstraint
        then return Nothing
        else Just <$> unsafeInsertTag toInsert tc

{- |
 Insert the given 'Tag` triple into the database.

 Other than the database-level unique constraints, performs no validation on the
 given triple.
-}
unsafeInsertTag ::
  (RecordKey File, RecordKey Descriptor, Maybe (RecordKey Tag)) ->
  TaggedConnection ->
  IO (RecordKey Tag)
unsafeInsertTag toInsert tc = do
  execute tc q toInsert
  lastInsertRowId tc
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