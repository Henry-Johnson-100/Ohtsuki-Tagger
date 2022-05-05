{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}

module Database.Tagger.Access
  ( Connection (..),
    FileKey (..),
    DescriptorKey (..),
    addFile,
    addDescriptor,
    addRepresentative,
    updateRepresentativeText,
    renameDescriptor,
    deleteDescriptor,
    insertDatabaseTag,
    fromDatabaseTag,
    newSubTags,
    getsDatabaseSubTags,
    deleteDatabaseTags,
    fromDatabaseSubTag,
    relate,
    unrelate,
    fetchInfraTree,
    fetchMetaTree,
    fetchMetaDescriptors,
    fetchInfraDescriptors,
    getDescriptor,
    getFile,
    getTagCount,
    getUntaggedFileWithTags,
    getRepresentative,
    lookupFileWithTagsByRelation,
    lookupFileWithTagsByFilePattern,
    lookupFileWithTagsByTagPattern,
    lookupFileWithTagByTagId,
    lookupFileWithTagsByFileId,
    lookupDescriptorPattern,
    hoistMaybe,
    activateForeignKeyPragma,
  )
where

import Control.Monad (unless, when, (>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List (foldl', isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes, fromJust, fromMaybe, maybe)
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Connection,
    FromRow,
    Only (Only),
    Query,
    execute,
    executeMany,
    execute_,
    lastInsertRowId,
    query,
    query_,
  )
import Database.SQLite.Simple.ToField (ToField)
import Database.Tagger.Access.RowMap
  ( fileWithMaybeTagsMapper,
    fileWithTagsMapper,
    tagCountMapper,
  )
import Database.Tagger.Type
  ( DatabaseSubTag (SubTag_),
    DatabaseTag (Tag_),
    Descriptor (Descriptor, descriptor, descriptorId),
    DescriptorKey,
    DescriptorTree (Infra),
    File (File, fileId),
    FileKey,
    FileWithTags (FileWithTags, tags),
    MetaDescriptor (..),
    Representative (Representative, repDescription),
    SubTag (SubTag),
    Tag (Tag),
    TagCount (..),
    TagKey,
    descriptorTreeElem,
    flattenTree,
    fwtFileEqual,
    insertIntoDescriptorTree,
    pushTag,
    tagId,
  )
import IO (hPutStrLn, stderr)
import Util.Core

debug# :: Bool
debug# = False

errout# :: String -> IO ()
errout# = when debug# . hPutStrLn stderr

erroutConcat# :: String -> String -> IO ()
erroutConcat# loc msg = errout# $ loc ++ msg

liftErroutConcat# :: MonadTrans t => String -> String -> t IO ()
liftErroutConcat# loc msg = lift . errout# $ loc ++ msg

activateForeignKeyPragma :: Connection -> IO ()
activateForeignKeyPragma c = execute_ c "PRAGMA foreign_keys = on"

-- | Attempts to add a new file to db
-- Performs no checking of the validity of a path.
addFile :: Connection -> T.Text -> IO File
addFile c f = do
  r <-
    execute
      c
      "INSERT INTO File (filePath) VALUES (?)"
      [f]
  insertedId <- lastInsertRowId c
  return . File (fromIntegral insertedId) $ f

insertDatabaseTag :: Connection -> DatabaseTag -> IO ()
insertDatabaseTag c (Tag_ _ f d) =
  execute
    c
    "INSERT INTO Tag (fileTagId, descriptorTagId) VALUES (?,?)"
    (f, d)

fromDatabaseTag :: Connection -> DatabaseTag -> MaybeT IO Tag
fromDatabaseTag c (Tag_ tk fk dk) = do
  f <- getFile c fk
  d <- getDescriptor c dk
  return $ Tag tk f d

newSubTags :: Connection -> [DatabaseSubTag] -> IO ()
newSubTags c =
  executeMany
    c
    "INSERT INTO SubTag (tagId, descriptorId) VALUES (?,?)"
    . map (\(SubTag_ tid did) -> (tid, did))

fromDatabaseSubTag :: Connection -> DatabaseSubTag -> MaybeT IO SubTag
fromDatabaseSubTag c (SubTag_ tk dk) = do
  dt <- getTag c tk
  t <- fromDatabaseTag c dt
  d <- getDescriptor c dk
  return (SubTag t d)

deleteDatabaseTags :: Connection -> [DatabaseTag] -> IO ()
deleteDatabaseTags c =
  executeMany c "DELETE FROM Tag WHERE fileTagId = ? AND descriptorTagId = ?"
    . map (\(Tag_ _ fk dk) -> (fk, dk))

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
    runMaybeT . mapM_ (unrelate c . descriptorId) $ infraRelations
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

deleteWhereIsInfraRelated :: Connection -> DescriptorKey -> IO ()
deleteWhereIsInfraRelated c =
  execute c "DELETE FROM MetaDescriptor WHERE infraDescriptorId = ?" . Only

-- | Delete all relations where a given key appears as infra (should only be 1)
-- And relate it to #UNRELATED#
unrelate :: Connection -> DescriptorKey -> MaybeT IO ()
unrelate c dk = do
  unrelatedDescriptor <- getUnrelatedDescriptor c
  result <- lift $ execute c "DELETE FROM MetaDescriptor WHERE infraDescriptorId = ?" [dk]
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

-- #TODO rename this
getUntaggedFileWithTags :: Connection -> IO [FileWithTags]
getUntaggedFileWithTags c = do
  r <-
    query_
      c
      "SELECT f.id, f.filePath \
      \FROM File f \
      \  LEFT JOIN Tag t \
      \    ON f.id = t.fileTagId \
      \WHERE t.descriptorTagId IS NULL"
  return . map (`FileWithTags` []) $ r

lookupFileWithTagsByRelation :: Connection -> DescriptorKey -> IO [FileWithTags]
lookupFileWithTagsByRelation c did = do
  relationTree <- runMaybeT . fetchInfraTree c $ did
  let maybeTags = map descriptorId . maybe [] flattenTree $ relationTree
  fmap concat . mapM (lookupFileWithTagByTagId c) $maybeTags

lookupFileWithTagsByFilePattern :: Connection -> T.Text -> IO [FileWithTags]
lookupFileWithTagsByFilePattern c p = do
  fs <- lookupFilePattern c p
  fmap concat . mapM (lookupFileWithTagsByFileId c . fileId) $ fs

lookupFileWithTagsByTagPattern :: Connection -> T.Text -> IO [FileWithTags]
lookupFileWithTagsByTagPattern c p = do
  ds <- lookupDescriptorPattern c p
  fmap concat . mapM (lookupFileWithTagByTagId c . descriptorId) $ ds

lookupFileWithTagByTagId :: Connection -> DescriptorKey -> IO [FileWithTags]
lookupFileWithTagByTagId conn t = do
  r <-
    query
      conn
      "SELECT \
      \f1.id, \
      \f1.filePath, \
      \d.id, \
      \d.descriptor \
      \FROM \
      \Tag t \
      \JOIN Descriptor d ON t.descriptorTagId = d.id \
      \JOIN ( \
      \SELECT \
      \f.* \
      \FROM \
      \File f \
      \JOIN Tag t ON f.id = t.fileTagId \
      \JOIN Descriptor d ON t.descriptorTagId = d.id \
      \WHERE \
      \d.id = ? \
      \) as f1 ON t.fileTagId = f1.id \
      \ORDER BY \
      \f1.filePath;"
      [t]
  return . fileWithTagsMapper $ r

lookupFileWithTagsByFileId :: Connection -> FileKey -> IO [FileWithTags]
lookupFileWithTagsByFileId c fid = do
  r <-
    query
      c
      "SELECT f.id, f.filePath, d.id, d.descriptor \
      \FROM File f \
      \  LEFT JOIN Tag t \
      \    ON f.id = t.fileTagId \
      \  LEFT JOIN Descriptor d \
      \    ON t.descriptorTagId = d.id \
      \WHERE f.id = ? \
      \ORDER BY f.filePath"
      [fid]
  return . fileWithMaybeTagsMapper $ r

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

lookupFilePattern :: Connection -> T.Text -> IO [File]
lookupFilePattern conn p = do
  r <-
    query
      conn
      "SELECT id, filePath \
      \FROM File \
      \WHERE filePath LIKE ?"
      [p]
  return r

{-
  ____ _____ _____
 / ___| ____|_   _|
| |  _|  _|   | |
| |_| | |___  | |
 \____|_____| |_|

Get functions take some primitive db value, either a key or link type like Tag
 and return a single MaybeT IO a of another primitive db value.

Gets functions take a list of primitive values and return IO [a] of another value.
-}

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

getsDatabaseSubTags :: Connection -> DatabaseTag -> IO [DatabaseSubTag]
getsDatabaseSubTags c (Tag_ tid _ _) =
  query
    c
    "SELECT tagId, descriptorId \
    \FROM SubTag \
    \WHERE tagId = ? \
    \ORDER BY tagId"
    [tid]

getTag :: Connection -> TagKey -> MaybeT IO DatabaseTag
getTag c tk = do
  r <-
    lift $
      query
        c
        "SELECT id, fileTagId, descriptorTagId \
        \FROM Tag WHERE id = ?"
        [tk] ::
      MaybeT IO [DatabaseTag]
  hoistMaybe . head' $ r

getTagCount :: Connection -> Descriptor -> IO TagCount
getTagCount c d = do
  result <- query c "SELECT COUNT(*) FROM Tag WHERE descriptorTagId = ?" [descriptorId d]
  return . tagCountMapper d . fromMaybe (Only 0 :: Only Int) . head' $ result