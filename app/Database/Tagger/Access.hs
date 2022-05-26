{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Tagger.Access
  ( Connection (..),
    FileKey (..),
    DescriptorKey (..),
    addFile,
    addDescriptor,
    addRepresentative,
    updateRepresentativeText,
    updateTagSubTagOfId,
    renameDescriptor,
    deleteDescriptor,
    insertDatabaseTag,
    derefTagPtr,
    deleteDatabaseSubTags,
    relate,
    unrelate,
    fetchInfraTree,
    fetchMetaTree,
    fetchMetaDescriptors,
    fetchInfraDescriptors,
    getDescriptor,
    getFile,
    getsUntaggedFileWithTags,
    getsDatabaseTags,
    getsDatabaseTagIds,
    getsExclusiveInfraDescriptorKeys,
    fromDatabaseFileWithTags,
    getRepresentative,
    getDescriptorOccurrenceMap,
    lookupFileWithTagsByFileId',
    lookupTagLike,
    lookupDescriptorPattern,
    hoistMaybe,
    activateForeignKeyPragma,
    lookupFilesHavingInfraTagRelationship,
    lookupFilesHavingTagKey,
    lookupFilesHavingFilePattern,
    lookupFilesHavingDescriptorKey,
    lookupFilesHavingDescriptorPattern,
    lookupFilesHavingNoTags,
    lookupFilesHavingSubTagRelationship,
    collectFileWithTagsByFileKey,
    derefDatabaseFileWithTags,
  )
where

import Control.Monad (unless, when, (>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.HashSet as HashSet
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', isPrefixOf, isSuffixOf)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Connection,
    Only (Only),
    Query (fromQuery),
    execute,
    executeMany,
    execute_,
    lastInsertRowId,
    query,
    query_,
  )
import Database.Tagger.Access.RowMap
  ( descriptorOccurrenceMapParser,
    reduceDbFwtList,
  )
import Database.Tagger.Type
  ( CollectedDatabaseFileWithTags (CollectedDatabaseFileWithTags),
    DatabaseFileWithTags (FileWithTags_),
    Descriptor (..),
    DescriptorKey,
    DescriptorTree (Infra),
    File (File, fileId),
    FileKey,
    FileWithTags (FileWithTags),
    MetaDescriptor (MetaDescriptor),
    QueryRequiring,
    Representative (Representative, repDescription),
    Tag (Tag),
    TagKey,
    TagNoId (TagNoId),
    TagPtr (..),
    TagPtrNoId (TagNoId_),
    databaseFileWithTagsFileKey,
    databaseFileWithTagsTagKeys,
    descriptorTreeElem,
    insertIntoDescriptorTree,
  )
import Event.Parser (PseudoDescriptor (PDescriptor))
import IO (hPutStrLn, stderr)
import Util.Core (OccurrenceMap, head', hoistMaybe, (!++))

-- | A literal WHERE clause, leaving out "WHERE"
newtype Where = Where Query deriving (Show, Eq)

-- | A literal SELECT clause, leaving out "SELECT" and the "FROM" clause
-- The FROM clause is set as a literal value in the CTE Query string.
newtype Select = Select Query deriving (Show, Eq)

debug# :: Bool
debug# = False

errout# :: String -> IO ()
errout# = when debug# . hPutStrLn stderr

liftErroutConcat# :: MonadTrans t => String -> String -> t IO ()
liftErroutConcat# loc msg = lift . errout# $ loc ++ msg

activateForeignKeyPragma :: Connection -> IO ()
activateForeignKeyPragma c = execute_ c "PRAGMA foreign_keys = on"

-- | Attempts to add a new file to db
-- Performs no checking of the validity of a path.
addFile :: Connection -> T.Text -> IO File
addFile c f = do
  _ <-
    execute
      c
      "INSERT INTO File (filePath) VALUES (?)"
      [f]
  insertedId <- lastInsertRowId c
  return . File (fromIntegral insertedId) $ f

insertDatabaseTag :: Connection -> TagPtrNoId -> IO TagKey
insertDatabaseTag c (TagNoId_ (Tag_ _ f d sid)) = do
  execute
    c
    "INSERT INTO Tag (fileId, descriptorId, subTagOfId) VALUES (?,?,?)"
    (f, d, sid)
  k <- lastInsertRowId c
  return . fromIntegral $ k

derefTagPtr :: Connection -> TagPtr -> MaybeT IO Tag
derefTagPtr c (Tag_ tk fk dk st) = do
  f <- getFile c fk
  d <- getDescriptor c dk
  return $ Tag tk f d st

updateTagSubTagOfId :: Connection -> TagPtr -> TagKey -> IO ()
updateTagSubTagOfId c t stid =
  execute
    c
    "UPDATE Tag SET subTagOfId = ? WHERE id = ?"
    (stid, (\(Tag_ tid _ _ _) -> tid) t)

-- | Deletes given tags using the tagId
-- and cascades the deletion to any tag that is a subtag.
deleteDatabaseSubTags :: Connection -> [TagPtr] -> IO ()
deleteDatabaseSubTags c dbts = do
  executeMany
    c
    "DELETE FROM Tag WHERE id = ? OR subTagOfId IS ?"
    . map (\(Tag_ tid _ _ _) -> (tid, tid))
    $ dbts

-- | Create new descriptor and relate it to #UNRELATED#
addDescriptor :: Connection -> T.Text -> MaybeT IO Descriptor
addDescriptor c dT = do
  let x #*# y = liftErroutConcat# "in Database.Tagger.Access.addDescriptor: " (x ++ y)
  "" #*# ""
  unrelatedDescriptor <- getUnrelatedDescriptor c
  "found unrelated: " #*# show unrelatedDescriptor
  lift
    . execute
      c
      "INSERT INTO Descriptor (descriptor) VALUES (?)"
    $ [dT]
  "Inserted new descriptor: " #*# T.unpack dT
  newDId <- fmap fromIntegral . lift . lastInsertRowId $ c
  "Got new Id: " #*# show newDId
  relate c (MetaDescriptor (descriptorId unrelatedDescriptor) newDId)
  "Created new unrelated Relation"
    #*# show (MetaDescriptor (descriptorId unrelatedDescriptor) newDId)
  return . Descriptor newDId $ dT

addRepresentative :: Connection -> Representative -> IO ()
addRepresentative c (Representative f d des) =
  execute
    c
    "INSERT INTO Representative (repFileId, repDescriptorId, description) \
    \ VALUES (?,?,?)"
    (fileId f, descriptorId d, des)

updateRepresentativeText ::
  Connection ->
  DescriptorKey ->
  T.Text ->
  IO ()
updateRepresentativeText c dk des =
  execute
    c
    "UPDATE Representative Set description = ? \
    \WHERE repDescriptorId = ?"
    (des, dk)

renameDescriptor :: Connection -> Descriptor -> T.Text -> IO ()
renameDescriptor c d n =
  execute
    c
    "UPDATE Descriptor SET descriptor = ? WHERE id = ?"
    (n, descriptorId d)

-- | Delete a descriptor from Descriptor.
-- Operates on (descriptorId :: Descriptor -> DescriptorKey)
--
-- The deletion in the Descriptor table should cascade to Tag and MetaDescriptor.
--
-- This function does a quick delete in those two tables anyways.
--
-- First unrelates any tags that are infra to the given descriptor.
-- So there are no null related descriptors left over.
--
-- Does nothing if the given descriptor is bookended by the char '#'
deleteDescriptor :: Connection -> Descriptor -> IO ()
deleteDescriptor c d = do
  let dstr = (T.unpack . descriptor) d
      (*#) = errout# . (++) "in Database.Tagger.Access.deleteDescriptor: "
  (*#) ""
  unless ("#" `isPrefixOf` dstr && "#" `isSuffixOf` dstr) $ do
    (*#) "in deleteDescriptor: "
    (*#) $ "in deleteDescriptor: trying deleting descriptor: " ++ show d
    unrelatedDescriptor <- runMaybeT . getUnrelatedDescriptor $ c
    (*#) $ "in deleteDescriptor: found unrelated " ++ show unrelatedDescriptor
    infraRelations <- fetchInfraDescriptors c . descriptorId $ d
    (*#) $ "in deleteDescriptor: found infra relations: " ++ show infraRelations
    _ <- runMaybeT . mapM_ (unrelate c . descriptorId) $ infraRelations
    (*#) "in deleteDescriptor: unrelated infra relations"
    execute c "DELETE FROM Descriptor WHERE id = ?" [descriptorId d]

relate :: Connection -> MetaDescriptor -> MaybeT IO ()
relate c md =
  case md of
    MetaDescriptor metaDK infraDK -> do
      let (*#) s = lift . errout# $ "in relate: " ++ s
      (*#) ""
      (*#) $ "relating: " ++ show md
      treeMetaToParent <- fetchMetaTree c metaDK
      (*#) $ "found treeMetaToParent: " ++ show treeMetaToParent
      infraDescriptor <- getDescriptor c infraDK
      (*#) $ "found infraDescriptor from key: " ++ show infraDescriptor
      unless
        (infraDescriptor `descriptorTreeElem` treeMetaToParent)
        ( do
            lift
              . execute
                c
                "INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId) \
                \VALUES (?,?)"
              $ (metaDK, infraDK)
            (*#) "Successfully inserted new relation."
        )
      return ()

-- deleteWhereIsInfraRelated :: Connection -> DescriptorKey -> IO ()
-- deleteWhereIsInfraRelated c =
--   execute c "DELETE FROM MetaDescriptor WHERE infraDescriptorId = ?" . Only

-- | Delete all relations where a given key appears as infra (should only be 1)
-- And relate it to #UNRELATED#
unrelate :: Connection -> DescriptorKey -> MaybeT IO ()
unrelate c dk = do
  unrelatedDescriptor <- getUnrelatedDescriptor c
  _ <- lift $ execute c "DELETE FROM MetaDescriptor WHERE infraDescriptorId = ?" [dk]
  relate c (MetaDescriptor (descriptorId unrelatedDescriptor) dk)

fetchMetaTree :: Connection -> DescriptorKey -> MaybeT IO DescriptorTree
fetchMetaTree c pid = do
  did <- getDescriptor c pid
  let parentTree = Infra did
  childrenTrees <-
    (lift . fetchMetaDescriptors c >=> mapM (fetchMetaTree c . descriptorId)) pid
  return . foldl' insertIntoDescriptorTree parentTree $ childrenTrees

fetchInfraTree :: Connection -> DescriptorKey -> MaybeT IO DescriptorTree
fetchInfraTree c pid = do
  did <- getDescriptor c pid
  let parentTree = Infra did
  childrenTrees <-
    ( lift . fetchInfraDescriptors c
        >=> mapM (fetchInfraTree c . descriptorId)
      )
      pid
  return . foldl' insertIntoDescriptorTree parentTree $ childrenTrees

fetchMetaDescriptors :: Connection -> DescriptorKey -> IO [Descriptor]
fetchMetaDescriptors c did = do
  r <-
    query
      c
      "SELECT d.id, d.descriptor \
      \FROM MetaDescriptor md \
      \  JOIN Descriptor d \
      \    ON md.metaDescriptorId = d.id \
      \WHERE md.infraDescriptorId = ?"
      [did]
  return r

fetchInfraDescriptors :: Connection -> DescriptorKey -> IO [Descriptor]
fetchInfraDescriptors c did = do
  r <-
    query
      c
      "SELECT d.id, d.descriptor \
      \FROM MetaDescriptor md \
      \  JOIN Descriptor d \
      \    ON md.infraDescriptorId = d.id \
      \WHERE md.metaDescriptorId = ?"
      [did]
  return r

-- This one is pobably better than the 'having' fks -> deref route
getsUntaggedFileWithTags :: Connection -> IO [FileWithTags]
getsUntaggedFileWithTags c = do
  r <-
    query_
      c
      "SELECT f.id, f.filePath \
      \FROM File f \
      \  LEFT JOIN Tag t \
      \    ON f.id = t.fileId \
      \WHERE t.descriptorId IS NULL"
  return . map (`FileWithTags` HashSet.empty) $ r

lookupFileWithTagsByFileId' :: Connection -> FileKey -> IO [DatabaseFileWithTags]
lookupFileWithTagsByFileId' c fk = do
  r <-
    query
      c
      "SELECT f.id, t.id \
      \FROM File f \
      \  LEFT JOIN Tag t \
      \    ON f.id = t.fileId \
      \WHERE f.id = ? \
      \ORDER BY f.filePath"
      [fk] ::
      IO [DatabaseFileWithTags]
  return . reduceDbFwtList $ r

lookupDescriptorPattern :: Connection -> T.Text -> IO [Descriptor]
lookupDescriptorPattern conn p = do
  r <-
    query
      conn
      "SELECT id, descriptor \
      \FROM Descriptor \
      \WHERE descriptor LIKE ?"
      [p]
  return r

fromDatabaseFileWithTags :: Connection -> DatabaseFileWithTags -> MaybeT IO FileWithTags
fromDatabaseFileWithTags c dbfwt = do
  f <- getFile c . databaseFileWithTagsFileKey $ dbfwt
  dbt <-
    fmap catMaybes
      . lift
      . mapM (runMaybeT . getTag c)
      . databaseFileWithTagsTagKeys
      $ dbfwt
  t <- fmap catMaybes . lift . mapM (runMaybeT . derefTagPtr c) $ dbt
  return . FileWithTags f . HashSet.fromList $ t

lookupTagLike :: Connection -> TagNoId -> MaybeT IO TagPtr
lookupTagLike c (TagNoId (Tag _ (File fk _) (Descriptor _ dt) _)) = do
  r <-
    lift $
      query
        c
        "SELECT t.id, t.fileId, t.descriptorId, t.subTagOfId \
        \FROM Tag t \
        \  JOIN Descriptor d \
        \    ON t.descriptorId = d.id \
        \WHERE t.fileId = ? AND d.descriptor LIKE ?"
        (fk, dt) ::
      MaybeT IO [TagPtr]
  hoistMaybe . head' $ r

{-
 _______        _________     _____ _______        __
|  ___\ \      / /_   _\ \   / /_ _| ____\ \      / /
| |_   \ \ /\ / /  | |  \ \ / / | ||  _|  \ \ /\ / /
|  _|   \ V  V /   | |   \ V /  | || |___  \ V  V /
|_|      \_/\_/    |_|    \_/  |___|_____|  \_/\_/
-}

lookupFilesHavingInfraTagRelationship :: Connection -> DescriptorKey -> IO [FileKey]
lookupFilesHavingInfraTagRelationship c dk = do
  mmd <- runMaybeT $ getDescriptor c dk
  maybe
    (return [])
    ( \md -> do
        mdfs <- lookupFilesHavingDescriptorKey c . descriptorId $ md
        let q =
              infraTreeRecursiveCTE
                &++ "SELECT DISTINCT infraDescriptorId FROM MD"
        infraR <-
          query
            c
            q
            [descriptorId md] ::
            IO [DescriptorKey]
        idfs <- fmap concat . mapM (lookupFilesHavingDescriptorKey c) $ infraR
        return . L.union mdfs $ idfs
    )
    mmd

lookupFilesHavingTagKey :: Connection -> TagKey -> IO [FileKey]
lookupFilesHavingTagKey c tk = do
  r <-
    query
      c
      "SELECT fileId \
      \FROM Tag \
      \WHERE id = ?"
      [tk] ::
      IO [TagKey]
  return r

lookupFilesHavingFilePattern :: Connection -> T.Text -> IO [FileKey]
lookupFilesHavingFilePattern c p = do
  r <-
    query
      c
      "SELECT id \
      \FROM File \
      \WHERE filePath LIKE ?"
      [p] ::
      IO [FileKey]
  return r

lookupFilesHavingDescriptorKey ::
  Connection -> DescriptorKey -> IO [FileKey]
lookupFilesHavingDescriptorKey c did = do
  r <-
    query
      c
      "SELECT DISTINCT fileId \
      \FROM Tag \
      \WHERE descriptorId = ? \
      \ORDER BY fileId"
      [did] ::
      IO [FileKey]
  return r

lookupFilesHavingDescriptorPattern ::
  Connection -> PseudoDescriptor -> IO [FileKey]
lookupFilesHavingDescriptorPattern c (PDescriptor p) = do
  r <-
    query
      c
      "SELECT DISTINCT t.fileId \
      \FROM Tag t \
      \  Join Descriptor d \
      \    ON t.descriptorId = d.id \
      \WHERE d.Descriptor LIKE ?"
      [p] ::
      IO [FileKey]
  return r

lookupFilesHavingNoTags :: Connection -> IO [CollectedDatabaseFileWithTags]
lookupFilesHavingNoTags c = do
  r <-
    query_
      c
      "SELECT id \
      \FROM FILE \
      \WHERE id NOT IN (SELECT fileId FROM Tag)" ::
      IO [FileKey]
  return . map (CollectedDatabaseFileWithTags . flip FileWithTags_ []) $ r

lookupFilesHavingSubTagRelationship ::
  Connection ->
  DescriptorKey ->
  [DescriptorKey] ->
  IO [FileKey]
lookupFilesHavingSubTagRelationship c mdk sdks = do
  let q =
        Select "DISTINCT fileId"
          `fromFileWithTagsCTE` Where
            ("t.descriptorId = ? AND st.descriptorId IN" &++ keysToLiteralSQLList sdks) ::
          QueryRequiring (Only DescriptorKey)
  query
    c
    q
    [mdk] ::
    IO [FileKey]

-- | SQL injection be damned
(&++) :: Query -> Query -> Query
qx &++ qy = fromString . T.unpack $ fromQuery qx !++ "\n" !++ fromQuery qy

-- | This is between 30-100x faster than before which is cool.
collectFileWithTagsByFileKey ::
  Connection -> [FileKey] -> IO [CollectedDatabaseFileWithTags]
collectFileWithTagsByFileKey c fks = do
  let q =
        Select "fileId, id"
          `fromFileWithTagsCTE` Where
            ("t.fileId IN" &++ keysToLiteralSQLList fks)
  r <-
    query_
      c
      q ::
      IO [DatabaseFileWithTags]
  return . map CollectedDatabaseFileWithTags . reduceDbFwtList $ r

derefDatabaseFileWithTags ::
  Connection ->
  CollectedDatabaseFileWithTags ->
  MaybeT IO FileWithTags
derefDatabaseFileWithTags c (CollectedDatabaseFileWithTags dbfwt) = do
  let tks = databaseFileWithTagsTagKeys dbfwt
      fk = databaseFileWithTagsFileKey dbfwt
  f <- getFile c fk
  let derefTk tk = do
        tPtr <- getTag c tk
        derefTagPtr c tPtr
  tags <- lift . fmap catMaybes . mapM (runMaybeT . derefTk) $ tks
  return . FileWithTags f . HashSet.fromList $ tags

keysToLiteralSQLList :: [Int] -> Query
keysToLiteralSQLList ks = fromString $ "(" ++ (L.intercalate ", " . map show) ks ++ ")"

getDescriptor :: Connection -> DescriptorKey -> MaybeT IO Descriptor
getDescriptor c did = do
  r <-
    lift $
      query
        c
        "SELECT id , descriptor FROM Descriptor WHERE id = ?"
        [did]
  hoistMaybe . head' $ r

getFile :: Connection -> FileKey -> MaybeT IO File
getFile c fid = do
  r <- lift $ query c "SELECT id, filePath FROM File WHERE id = ?" [fid]
  hoistMaybe . head' $ r

getUnrelatedDescriptor :: Connection -> MaybeT IO Descriptor
getUnrelatedDescriptor =
  lift . flip lookupDescriptorPattern "#UNRELATED#" >=> hoistMaybe . head'

getRepresentative :: Connection -> DescriptorKey -> MaybeT IO Representative
getRepresentative c descriptorId = do
  r' <-
    lift $
      query
        c
        "SELECT repFileId, repDescriptorId, description \
        \FROM Representative \
        \WHERE repDescriptorId = ?"
        [descriptorId] ::
      MaybeT IO [(Int, Int, Maybe T.Text)]
  r <- hoistMaybe . head' $ r'
  rep <- do
    f <- getFile c . (\(k, _, _) -> k) $ r
    d <- getDescriptor c . (\(_, k, _) -> k) $ r
    return $ Representative f d Nothing
  return $ rep {repDescription = (\(_, _, d') -> d') r}

getTag :: Connection -> TagKey -> MaybeT IO TagPtr
getTag c tk = do
  r <-
    lift $
      query
        c
        "SELECT id, fileId, descriptorId, subTagOfId \
        \FROM Tag WHERE id = ?"
        [tk] ::
      MaybeT IO [TagPtr]
  hoistMaybe . head' $ r

-- | Gets a list of Database tags by searching via
-- the fileId, descriptorId, and subTagOfId of a Tag.
--
-- A valid tagId is not used.
getsDatabaseTags :: Connection -> TagNoId -> IO [TagPtr]
getsDatabaseTags c (TagNoId (Tag _ fid did sid)) =
  query
    c
    "SELECT id, fileId, descriptorId, subTagOfId \
    \FROM Tag WHERE fileId = ? AND descriptorId = ? AND subTagOfId IS ?"
    (fileId fid, descriptorId did, sid) ::
    IO [TagPtr]

getsDatabaseTagIds :: Connection -> [TagPtrNoId] -> IO [TagPtr]
getsDatabaseTagIds c dbts = do
  let q =
        query
          c
          "SELECT id, fileId, descriptorId, subTagOfId \
          \FROM Tag \
          \WHERE fileId = ? AND descriptorId = ? AND subTagOfId IS ?"
          . (\(TagNoId_ (Tag_ _ fid did mstid)) -> (fid, did, mstid)) ::
          TagPtrNoId -> IO [TagPtr]
  fmap concat . mapM q $ dbts

getDescriptorOccurrenceMap ::
  Connection -> [DescriptorKey] -> IO (OccurrenceMap Descriptor)
getDescriptorOccurrenceMap c dks = do
  let q =
        fmap (map descriptorOccurrenceMapParser)
          . query
            c
            "SELECT d.id, count(d.id) \
            \FROM Tag t \
            \  JOIN Descriptor d \
            \    ON t.descriptorId = d.id \
            \WHERE t.descriptorId = ? \
            \GROUP BY d.id"
          . Only ::
          DescriptorKey -> IO [OccurrenceMap Descriptor]
  r <- fmap concat . mapM q $ dks
  return . IntMap.unions $ r

getsExclusiveInfraDescriptorKeys :: Connection -> DescriptorKey -> IO [DescriptorKey]
getsExclusiveInfraDescriptorKeys c dk = do
  let q =
        infraTreeRecursiveCTE
          &++ "SELECT DISTINCT infraDescriptorId \
              \FROM MD" ::
          QueryRequiring (Only DescriptorKey)
  query
    c
    q
    [dk] ::
    IO [DescriptorKey]

infraTreeRecursiveCTE :: QueryRequiring (Only DescriptorKey)
infraTreeRecursiveCTE =
  "WITH RECURSIVE MD AS ( \
  \  SELECT \
  \    md.id \"metaDescriptorId\" \
  \    ,md.descriptor \"metaDescriptor\" \
  \    ,id.id \"infraDescriptorId\" \
  \    ,id.descriptor \"infraDescriptor\" \
  \  FROM MetaDescriptor meta \
  \    JOIN Descriptor md \
  \      ON meta.metaDescriptorId = md.id \
  \    JOIN Descriptor id \
  \      ON meta.infraDescriptorId = id.id \
  \  WHERE metaDescriptorId = ? \
  \  UNION ALL \
  \  SELECT \
  \    mdv.infraDescriptorId \
  \    ,mdv.infraDescriptor \
  \    ,d.id \
  \    ,d.descriptor \
  \  FROM MD mdv \
  \    JOIN MetaDescriptor meta \
  \      ON mdv.infraDescriptorId = meta.metaDescriptorId \
  \    JOIN Descriptor d \
  \      ON meta.infraDescriptorId = d.id \
  \)"

-- | A recursive CTE "FileWithTags" that has a granularity of one row per tag.
--
-- Columns:
--
-- t.id                                 - TagKey
--
-- t.subTagOfId     "subTagOfId"        - TagKey
--
-- st.id            "subTagId"          - TagKey
--
-- t.fileId                             - FileKey
--
-- t.descriptorId                       - DescriptorKey
--
-- st.descriptorId "subTagDescriptorId" - DescriptorKey
fromFileWithTagsCTE :: Select -> Where -> Query
fromFileWithTagsCTE (Select selectClause) (Where whereClause) =
  "WITH RECURSIVE FileWithTags AS ( \
  \  SELECT \
  \    t.id \
  \    ,t.subTagOfId \"subTagOfId\" \
  \    ,st.id \"subTagId\" \
  \    ,t.fileId \
  \    ,t.descriptorId \
  \    ,st.descriptorId \"subTagDescriptorId\" \
  \  FROM Tag t \
  \    LEFT JOIN Tag st \
  \      ON t.id = st.subTagOfId \
  \WHERE"
    &++ whereClause
    &++ "  UNION \
        \  SELECT \
        \    fwt.id \
        \    ,fwt.subTagOfId \
        \    ,st.id \"subTagId\" \
        \    ,fwt.fileId \
        \    ,fwt.descriptorId \
        \    ,st.descriptorId \"subTagDescriptorId\" \
        \    FROM FileWithTags fwt \
        \      LEFT JOIN Tag st \
        \        ON fwt.id = st.subTagOfId \
        \) \
        \SELECT"
    &++ selectClause
    &++ "FROM FileWithTags"
