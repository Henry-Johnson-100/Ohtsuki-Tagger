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
    renameDescriptor,
    deleteDescriptor,
    newTag,
    untag,
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
import Data.Maybe (catMaybes, fromJust, fromMaybe)
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
import Database.Tagger.Type
  ( Descriptor (Descriptor, descriptor, descriptorId),
    DescriptorTree (Infra),
    File (File, fileId),
    FileWithTags (FileWithTags, tags),
    MetaDescriptor (..),
    Tag (..),
    TagCount (..),
    descriptorTreeElem,
    flattenTree,
    fwtFileEqual,
    insertIntoDescriptorTree,
    pushTag,
  )
import IO (hPutStrLn, stderr)

debug# :: Bool
debug# = False

errout# :: String -> IO ()
errout# = when debug# . hPutStrLn stderr

erroutConcat# :: String -> String -> IO ()
erroutConcat# loc msg = errout# $ loc ++ msg

liftErroutConcat# :: MonadTrans t => String -> String -> t IO ()
liftErroutConcat# loc msg = lift . errout# $ loc ++ msg

type FileKey = Int

type DescriptorKey = Int

activateForeignKeyPragma :: Connection -> IO ()
activateForeignKeyPragma c = execute_ c "PRAGMA foreign_keys = on"

-- | Check if all the required tables are present.
-- And if the 3 required meta descriptors are set up properly.
validateDb :: Connection -> IO Bool
validateDb c = do
  tables <-
    fmap (all (onlyIntEquals 4))
      . (query_ :: Connection -> Query -> IO [Only Int])
        c
      $ "SELECT COUNT(*) \
        \FROM sqlite_master \
        \WHERE type = 'table' \
        \  AND tbl_name IN ('Descriptor','File','MetaDescriptor','Tag')"
  errout# $ "has valid number of tables: " ++ show tables
  requiredMetaDescriptors <-
    fmap (all (onlyIntEquals 3))
      . (query_ :: Connection -> Query -> IO [Only Int])
        c
      $ "SELECT COUNT(*) FROM Descriptor \
        \WHERE descriptor IN ('#ALL#','#UNRELATED#','#META#')"
  errout# $ "Has valid number of meta descriptors: " ++ show requiredMetaDescriptors
  requiredRelations <-
    fmap (all (onlyIntEquals 2))
      . (query_ :: Connection -> Query -> IO [Only Int]) c
      $ "SELECT COUNT(*) \
        \FROM MetaDescriptor \
        \WHERE (metaDescriptorId = \
        \    (SELECT id FROM Descriptor WHERE descriptor = '#ALL#') \
        \    AND infraDescriptorId = \
        \      (SELECT id FROM Descriptor WHERE descriptor = '#META#')) \
        \  OR (metaDescriptorId = \
        \      (SELECT id FROM Descriptor WHERE descriptor = '#ALL#') \
        \    AND infraDescriptorId = \
        \      (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#'))"
  errout# $ "Has valid number of predefined relations: " ++ show requiredRelations
  return $ tables && requiredMetaDescriptors && requiredRelations
  where
    onlyIntEquals :: Eq a => a -> Only a -> Bool
    onlyIntEquals n (Only m) = n == m

getTagCount :: Connection -> Descriptor -> IO TagCount
getTagCount c d = do
  result <- query c "SELECT COUNT(*) FROM Tag WHERE descriptorTagId = ?" [descriptorId d]
  return . mapQToTagCount d . fromMaybe (Only 0 :: Only Int) . head' $ result

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

newTag :: Connection -> Tag -> IO ()
newTag c (Tag f d) =
  execute
    c
    "INSERT INTO Tag (fileTagId, descriptorTagId) VALUES (?,?)"
    (f, d)

untag :: Connection -> [Tag] -> IO ()
untag c =
  executeMany c "DELETE FROM Tag WHERE fileTagId = ? AND descriptorTagId = ?"
    . map encurryTag
  where
    encurryTag :: Tag -> (FileKey, DescriptorKey)
    encurryTag (Tag fk dk) = (fk, dk)

getUnrelatedDescriptor :: Connection -> MaybeT IO Descriptor
getUnrelatedDescriptor =
  lift . flip lookupDescriptorPattern "#UNRELATED#" >=> hoistMaybe . head'

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

-- #TODO no task or event assigned
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
  return . map mapQToDescriptor $ r

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
  return . map mapQToDescriptor $ r

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

getDescriptor :: Connection -> DescriptorKey -> MaybeT IO Descriptor
getDescriptor c did = do
  r <-
    lift $
      query
        c
        "SELECT id , descriptor FROM Descriptor WHERE id = ?"
        [did]
  hoistMaybe . fmap mapQToDescriptor . head' $ r

getFile :: Connection -> FileKey -> MaybeT IO File
getFile c fid = do
  r <- lift $ query c "SELECT id, filePath FROM File WHERE id = ?" [fid]
  hoistMaybe . fmap mapQToFile . head' $ r

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
  let files = map mapQToFile r
  return . map (`FileWithTags` []) $ files

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
  return . groupRFWT [] $ r

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
  return . groupRFWMT [] $ r

lookupDescriptorPattern :: Connection -> T.Text -> IO [Descriptor]
lookupDescriptorPattern conn p = do
  r <-
    query
      conn
      "SELECT id, descriptor \
      \FROM Descriptor \
      \WHERE descriptor LIKE ?"
      [p]
  return . map mapQToDescriptor $ r

lookupFilePattern :: Connection -> T.Text -> IO [File]
lookupFilePattern conn p = do
  r <-
    query
      conn
      "SELECT id, filePath \
      \FROM File \
      \WHERE filePath LIKE ?"
      [p]
  return . map mapQToFile $ r

mapQToFile :: (Int, T.Text) -> File
mapQToFile (fid, fpath) = File fid fpath

mapQToDescriptor :: (Int, T.Text) -> Descriptor
mapQToDescriptor (did, d) = Descriptor did d

groupRFWT :: [FileWithTags] -> [(Int, T.Text, Int, T.Text)] -> [FileWithTags]
groupRFWT accum [] = accum
groupRFWT [] (r : rs) = groupRFWT [mapQToFWT r] rs
groupRFWT (old : past) (r : rs) =
  let new = mapQToFWT r
   in if new `fwtFileEqual` old
        then groupRFWT (foldl' pushTag old (tags new) : past) rs
        else groupRFWT (new : old : past) rs

-- | For grouping FWTs that have Maybe Tags
groupRFWMT :: [FileWithTags] -> [(Int, T.Text, Maybe Int, Maybe T.Text)] -> [FileWithTags]
groupRFWMT accum [] = accum
groupRFWMT [] (r : rs) = groupRFWMT [mapQToFWMT r] rs
groupRFWMT (old : past) (r : rs) =
  let new = mapQToFWMT r
   in if new `fwtFileEqual` old
        then groupRFWMT (foldl' pushTag old (tags new) : past) rs
        else groupRFWMT (new : old : past) rs

mapQToFWMT :: (Int, T.Text, Maybe Int, Maybe T.Text) -> FileWithTags
mapQToFWMT (fid, fp, dids, dds) =
  FileWithTags (File fid fp) (maybe [] (\d -> [Descriptor (fromJust dids) d]) dds)

mapQToFWT :: (Int, T.Text, Int, T.Text) -> FileWithTags
mapQToFWT (fid, fp, dids, dds) = FileWithTags (File fid fp) [Descriptor dids dds]

mapQToTagCount :: Descriptor -> Only Int -> TagCount
mapQToTagCount d (Only n) = (d, n)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x