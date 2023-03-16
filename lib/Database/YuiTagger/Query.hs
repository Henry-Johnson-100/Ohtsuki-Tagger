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
Module      : Database.YuiTagger.Query.Basic
Description : Contains basic queries.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Contains basic queries such as searching for files by file pattern or searching for
Descriptor by Relation.

Some other modules may contain SELECT queries that are specific to them,
but NO modules other than this contain commands that may modify the contents
of any of the tables outside of the 'TaggerDBInfo` table.
-}
module Database.YuiTagger.Query (
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

  -- *** On 'Tag`
  queryForFileByTagId,

  -- ** 'TaggedFile` and 'ConcreteTaggedFile` Queries
  derefTag,
  queryForFileTagHierarchyMapByFileId,
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

  -- *** On 'File`
  queryForDescriptorByFileId,

  -- ** 'Tag` Queries

  -- | Queries that return 'Tag`s.
  allTags,
  queryForTagByFileAndDescriptorKey,
  queryForTagByFileAndDescriptorKeyAndNullSubTagOf,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagBySubTagTriple,

  -- *** On 'Descriptor`
  queryForTagByDescriptorPattern,
  queryForTagByMetaDescriptorPattern,

  -- *** On 'File`
  queryForFileTagsByFileId,

  -- ** MetaDescriptor Queries

  -- | Queries on the MetaDescriptor table. Used in testing.
  allMetaDescriptorRows,

  -- ** Misc.
  hasInfraRelations,
  getTagOccurrencesByDescriptorKeys,
  getLastAccessed,
  getLastSaved,
  getDirectories,

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
  deleteTags,
  unSubTags,
  moveSubTags,

  -- *** 'Tag` Insertion
  -- $FailedInsertion
  insertTags,
  unsafeInsertTag,
) where

import Control.Monad (join)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bifunctor (second)
import qualified Data.HashSet as HashSet
import Data.HierarchyMap (HierarchyMap)
import qualified Data.HierarchyMap as HAM
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.OccurrenceMap.Internal as OM
import Data.Text (Text)
import qualified Data.Text as T
import Database.YuiTagger.Connection (
  NamedParam ((:=)),
  Only (Only),
  execute,
  executeMany,
  executeNamed,
  lastInsertRowId,
  query,
  queryNamed,
  query_,
 )
import Database.YuiTagger.Type (
  ConcreteTag (ConcreteTag),
  ConcreteTaggedFile (ConcreteTaggedFile),
  Descriptor (Descriptor),
  File,
  RecordKey,
  Tag (Tag, tagSubtagOfId),
  TaggedConnection,
  filePath,
 )
import qualified System.FilePath as FilePath
import YuiTagger.Util (catMaybeTM, head', hoistMaybe)
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
queryForSingleFileByFileId :: RecordKey File -> TaggedConnection -> IO (Maybe File)
queryForSingleFileByFileId rk tc = head' <$> query tc q [rk]
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

{- |
 Retrieve the 'File` that is tagged with the given 'Tag` if it exists.
-}
queryForFileByTagId :: RecordKey Tag -> TaggedConnection -> IO (Maybe File)
queryForFileByTagId rt tc = head' <$> query tc q [rt]
 where
  q =
    [r|
      SELECT
        f.id
        ,f.filePath
      FROM File f
      JOIN Tag t ON f.id = t.fileId
      WHERE t.id = ?
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
 Dereference a 'Tag` to a 'ConcreteTag` from its ID.
-}
derefTag :: RecordKey Tag -> TaggedConnection -> MaybeT IO ConcreteTag
derefTag tid tc = do
  results <- lift $ query tc q [tid]
  hoistMaybe . fmap uncurryConcreteTag . head' $ results
 where
  uncurryConcreteTag (tid', did, dp, mstid) = ConcreteTag tid' (Descriptor did dp) mstid
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
 Given a 'File` ID, return a 'HierarchyMap` of its 'Tag`s.

 The map is empty if the 'File` does not exist or if it has no 'Tag`s.
-}
queryForFileTagHierarchyMapByFileId ::
  RecordKey File ->
  TaggedConnection ->
  IO (HierarchyMap ConcreteTag)
queryForFileTagHierarchyMapByFileId fk tc = do
  fileTags <- queryForFileTagsByFileId fk tc
  concreteTagRelationTuples <- catMaybeTM (derefRelationTuple . relationTuple) fileTags
  return . HAM.fromList . map (second HashSet.fromList) $ concreteTagRelationTuples
 where
  relationTuple :: Tag -> (RecordKey Tag, [RecordKey Tag])
  relationTuple (Tag t _ _ ms) =
    maybe (t, []) (,[t]) ms
  derefRelationTuple ::
    (RecordKey Tag, [RecordKey Tag]) ->
    MaybeT IO (ConcreteTag, [ConcreteTag])
  derefRelationTuple (x, xs) =
    (,)
      <$> derefTag x tc
      <*> (lift . catMaybeTM (`derefTag` tc) $ xs)

{- |
 Given a 'File` ID, return the corresponding 'ConcreteTaggedFile` if it exists.

 Intended to be called for only one file at a time. Calling it for large numbers of
  'File`s is quite costly and provides no real benefit in the YuiTagger GUI front-end.
-}
queryForConcreteTaggedFileWithFileId ::
  RecordKey File ->
  TaggedConnection ->
  MaybeT IO ConcreteTaggedFile
queryForConcreteTaggedFileWithFileId rk tc =
  ConcreteTaggedFile
    <$> MaybeT (queryForSingleFileByFileId rk tc)
    <*> lift (queryForFileTagHierarchyMapByFileId rk tc)

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
  Doing so may cause some YuiTagger operations to fail.
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

 The new 'Descriptor`s will automatically be related to \#UNRELATED\# via
  a trigger that fires on @INSERT ON Descriptor@.
-}
insertDescriptors :: [T.Text] -> TaggedConnection -> IO ()
insertDescriptors ps tc = do
  executeMany tc q (Only <$> ps)
 where
  q =
    [r|
    INSERT INTO Descriptor (descriptor) VALUES (?)
    |]

{- |
 Delete a list of 'Descriptor`s from the database.

 Triggers a maintenance execution on the db, inserting all 'Descriptor`s that are not
 infra to anything as infra to \#UNRELATED\#.
-}
deleteDescriptors :: [RecordKey Descriptor] -> TaggedConnection -> IO ()
deleteDescriptors dks tc = do
  executeMany tc q (Only <$> dks)
 where
  q =
    [r|
      DELETE FROM Descriptor WHERE id = ?
      |]

{- |
 Given a tuple of 'Text` and a 'Descriptor`'s primary key, relabel that 'Descriptor`.
-}
updateDescriptors :: [(T.Text, RecordKey Descriptor)] -> TaggedConnection -> IO ()
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
 in a YuiTagger's database.
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
 Get all 'Descriptor`s that are tagged to the given 'File`.

 Can contain duplicates.
-}
queryForDescriptorByFileId :: RecordKey File -> TaggedConnection -> IO [Descriptor]
queryForDescriptorByFileId fk tc = query tc q [fk]
 where
  q =
    [r|
    SELECT
      d.id
      ,d.descriptor
    FROM Tag t
    JOIN Descriptor d ON t.descriptorId = d.id
    WHERE t.fileId = ?
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
 Returns a sorted list of all directories of files stored in the database.
-}
getDirectories :: TaggedConnection -> IO [FilePath]
getDirectories =
  fmap
    (L.sort . L.nub . map (FilePath.dropFileName . T.unpack . filePath))
    . allFiles

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
 Retrieve all 'Tag`s corresponding to 'Descriptor`s that match
 the given text pattern.
-}
queryForTagByDescriptorPattern :: Text -> TaggedConnection -> IO [Tag]
queryForTagByDescriptorPattern t tc = query tc q [t]
 where
  q =
    [r|
      SELECT
        t.id,
        t.fileId,
        t.descriptorId,
        t.subTagOfId
      FROM Tag t
      JOIN Descriptor d ON t.descriptorId = d.id
      WHERE d.descriptor LIKE ? ESCAPE '\'|]

{- |
 Retrieve all 'Tag`s corresponding to all 'Descriptor`s that are
 infra to the 'Descriptor`s matching the given text pattern.
-}
queryForTagByMetaDescriptorPattern :: Text -> TaggedConnection -> IO [Tag]
queryForTagByMetaDescriptorPattern t tc = query tc q [t]
 where
  q =
    [r|
        SELECT
          t.id
          ,t.fileId
          ,t.descriptorId
          ,t.subTagOfId
        FROM Tag t
        JOIN (
          WITH RECURSIVE r(id) AS (
            SELECT id
            FROM Descriptor
            WHERE descriptor LIKE ? ESCAPE '\'
            UNION
            SELECT infraDescriptorId
            FROM MetaDescriptor md
            JOIN r ON md.metaDescriptorId = r.id
          )
          SELECT id FROM r
        ) AS d ON t.descriptorId = d.id|]

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
