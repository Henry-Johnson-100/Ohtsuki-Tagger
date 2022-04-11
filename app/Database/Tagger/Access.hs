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
    deleteDescriptor,
    newTag,
    relate,
    unrelate,
    deleteRelation,
    fetchInfraTree,
    fetchMetaTree,
    fetchMetaDescriptors,
    fetchInfraDescriptors,
    getDescriptor,
    getFile,
    getUntaggedFileWithTags,
    lookupFileWithTagsByRelation,
    lookupFileWithTagsByFilePattern,
    lookupFileWithTagsByTagPattern,
    lookupFileWithTagByTagId,
    lookupFileWithTagsByFileId,
    lookupDescriptorPattern,
    hoistMaybe,
  )
where

import Control.Monad (unless, when, (>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List (foldl', isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Connection,
    FromRow,
    Only (Only),
    execute,
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
    descriptorTreeElem,
    flattenTree,
    fwtFileEqual,
    insertIntoDescriptorTree,
    pushTag,
    validatePath,
  )
import System.IO (hPutStrLn, stderr)

debug# :: Bool
debug# = False

errout# :: String -> IO ()
errout# = when debug# . hPutStrLn stderr

type FileKey = Int

type DescriptorKey = Int

-- | Attempts to add a new file to db
-- Should do nothing if the file is not valid
addFile :: Connection -> T.Text -> MaybeT IO File
addFile c f = do
  validatedPath <- validatePath f
  r <-
    lift $
      execute
        c
        "INSERT INTO File (filePath) VALUES (?)"
        [validatedPath]
  insertedId <- lift . lastInsertRowId $ c
  return . File (fromIntegral insertedId) $ validatedPath

newTag :: Connection -> Tag -> IO ()
newTag c (Tag f d) =
  execute
    c
    "INSERT INTO Tag (fileTagId, descriptorTagId) VALUES (?,?)"
    (f, d)

getUnrelatedDescriptor :: Connection -> MaybeT IO Descriptor
getUnrelatedDescriptor =
  lift . flip lookupDescriptorPattern "#UNRELATED#" >=> hoistMaybe . head'

-- | Create new descriptor and relate it to #UNRELATED#
addDescriptor :: Connection -> T.Text -> MaybeT IO Descriptor
addDescriptor c dT = do
  lift . errout# $ "in addDescriptor: "
  unrelatedDescriptor <- getUnrelatedDescriptor c
  lift . errout# $ "in addDescriptor: found unrelated: " ++ show unrelatedDescriptor
  lift
    . execute
      c
      "INSERT INTO Descriptor (descriptor) VALUES (?)"
    $ [dT]
  lift . errout# $ "in addDescriptor: Inserted new descriptor: " ++ T.unpack dT
  newDId <- fmap fromIntegral . lift . lastInsertRowId $ c
  lift . errout# $ "in addDescriptor: Got new Id: " ++ show newDId
  relate c (MetaDescriptor (descriptorId unrelatedDescriptor) newDId)
  lift . errout# $
    "in addDescriptor: Created new unrelated Relation"
      ++ show (MetaDescriptor (descriptorId unrelatedDescriptor) newDId)
  return . Descriptor newDId $ dT

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
  unless ("#" `isPrefixOf` dstr && "#" `isSuffixOf` dstr) $ do
    errout# "in deleteDescriptor: "
    errout# $ "in deleteDescriptor: trying deleting descriptor: " ++ show d
    unrelatedDescriptor <- runMaybeT . getUnrelatedDescriptor $ c
    errout# $ "in deleteDescriptor: found unrelated " ++ show unrelatedDescriptor
    infraRelations <- fetchInfraDescriptors c . descriptorId $ d
    errout# $ "in deleteDescriptor: found infra relations: " ++ show infraRelations
    runMaybeT . mapM_ (unrelate c . descriptorId) $ infraRelations
    errout# "in deleteDescriptor: unrelated infra relations"
    execute c "DELETE FROM Descriptor WHERE id = ?" [descriptorId d]
    errout# "in deleteDescriptor: deleted from Descriptor"
    execute c "DELETE FROM Tag WHERE descriptorTagId = ?" [descriptorId d]
    errout# "in deleteDescriptor: deleted tags"
    execute
      c
      "DELETE FROM MetaDescriptor WHERE metaDescriptorId = ? OR infraDescriptorId = ?"
      (descriptorId d, descriptorId d)
    errout# "in deleteDescriptor: deleted all relations"

relate :: Connection -> MetaDescriptor -> MaybeT IO ()
relate c md =
  case md of
    MetaDescriptor metaDK infraDK -> do
      let dmsg# s = lift . errout# $ "in relate: " ++ s
      dmsg# ""
      dmsg# $ "relating: " ++ show md
      treeMetaToParent <- fetchMetaTree c metaDK
      dmsg# $ "found treeMetaToParent: " ++ show treeMetaToParent
      infraDescriptor <- getDescriptor c infraDK
      dmsg# $ "found infraDescriptor from key: " ++ show infraDescriptor
      unless
        (infraDescriptor `descriptorTreeElem` treeMetaToParent)
        ( do
            lift . deleteWhereIsInfraRelated c $ infraDK
            dmsg# $
              "Deleted all relations where, "
                ++ show infraDescriptor
                ++ ", was infra"
            lift
              . execute
                c
                "INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId) \
                \VALUES (?,?)"
              $ (metaDK, infraDK)
            dmsg# "Successfully inserted new relation."
        )
      return ()

-- deleteWhereIsInfraRelated :: Connection -> DescriptorKey ->
deleteWhereIsInfraRelated c =
  execute c "DELETE FROM MetaDescriptor WHERE infraDescriptorId = ?" . Only

isInfraToAny :: Connection -> DescriptorKey -> IO Bool
isInfraToAny c k = do
  let dmsg# = errout# . (++) "in isInfraToAny: "
  dmsg# ""
  dmsg# $ "Checking if descriptorKey, " ++ show k ++ " is Infra to any."
  r <-
    query
      c
      "SELECT metaDescriptorId FROM MetaDescriptor \
      \WHERE infraDescriptorId = ?"
      (Only k) ::
      IO [Only Int]
  dmsg# $ "Found the following infra relationships: " ++ show r
  return . not . null $ r

hasSpecificRelation :: Connection -> MetaDescriptor -> IO Bool
hasSpecificRelation c (MetaDescriptor mdk idk) = do
  let dmsg# = errout# . (++) "In hasSpecificRelation: "
  dmsg# ""
  dmsg# $ "Looking for relation: " ++ show (MetaDescriptor mdk idk)
  r <-
    query
      c
      "SELECT * FROM MetaDescriptor \
      \WHERE metaDescriptorId = ? AND infraDescriptorId = ?"
      (mdk, idk) ::
      IO [(Int, Int)]
  dmsg# $ "Found relations: " ++ show r
  return . not . null $ r

deleteRelation :: Connection -> MetaDescriptor -> IO ()
deleteRelation c (MetaDescriptor mdk idk) =
  execute
    c
    "DELETE FROM MetaDescriptor \
    \WHERE metaDescriptorId = ? AND infraDescriptorId = ?"
    (mdk, idk)

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
      \FROM Tag t \
      \  JOIN File f \
      \    ON t.fileTagId = f.id \
      \  JOIN Descriptor d \
      \    ON t.descriptorTagId = d.id \
      \WHERE t.fileTagId = ? \
      \ORDER BY f.filePath"
      [fid]
  return . groupRFWT [] $ r

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

mapQToFWT :: (Int, T.Text, Int, T.Text) -> FileWithTags
mapQToFWT (fid, fp, dids, dds) = FileWithTags (File fid fp) [Descriptor dids dds]

data TestFWT = T Int T.Text Int T.Text deriving (Show, Eq)

tmap (fid, fp, dids, dds) = T fid fp dids dds

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x