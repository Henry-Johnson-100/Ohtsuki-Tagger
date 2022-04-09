{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}

module Database.TaggerNew.Access
  ( addFile,
    addDescriptor,
    tagFileWithDescriptor,
    relate,
    deleteRelation,
    fetchInfraTree,
    fetchMetaDescriptors,
    fetchInfraDescriptors,
    getDescriptor,
    getFile,
    getUntaggedFileWithTags,
    lookupFileWithTagsByRelation,
    lookupFileWithTagsByFilePattern,
    lookupFileWithTagByTagId,
    lookupFileWithTagsByFileId,
    lookupDescriptorPattern,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.TaggerNew.Type

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

tagFileWithDescriptor :: Connection -> File -> Descriptor -> IO ()
tagFileWithDescriptor c f d =
  execute
    c
    "INSERT INTO Tag (fileTagId, descriptorTagId) VALUES (?,?)"
    (fileId f, descriptorId d)

getUnrelatedDescriptor :: Connection -> MaybeT IO Descriptor
getUnrelatedDescriptor =
  lift . flip lookupDescriptorPattern "#UNRELATED#" >=> hoistMaybe . head'

-- | Create new descriptor and relate it to #UNRELATED#
addDescriptor :: Connection -> T.Text -> MaybeT IO Descriptor
addDescriptor c dT = do
  unrelatedDescriptor <- getUnrelatedDescriptor c
  lift
    . execute
      c
      "INSERT INTO Descriptor (descriptor) VALUES (?)"
    $ [dT]
  newDId <- fmap fromIntegral . lift . lastInsertRowId $ c
  relate c (MetaDescriptor (descriptorId unrelatedDescriptor) newDId)
  return . Descriptor newDId $dT

-- | Creates a relation iff these criteria are met:
--
-- - The InfraDescriptor is not present in the tree that is meta to the MetaDescriptor.
--  To avoid recursive relations
--
-- - The InfraDescriptor is not Infra to any other Descriptor
--
-- With the exception of being Infra to \#UNRELATED\#
--  - If it is related to \#UNRELATED\# This relationship is deleted before checking for
-- any other relationships.
relate :: Connection -> MetaDescriptor -> MaybeT IO ()
relate c md =
  case md of
    MetaDescriptor metaDK infraDK -> do
      treeMetaToParent <- fetchMetaTree c metaDK
      infraDescriptor <- getDescriptor c infraDK
      unless
        (infraDescriptor `descriptorTreeElem` treeMetaToParent)
        ( do
            unrelatedMetaDescriptor <- getUnrelatedDescriptor c
            let unrelatedRelation =
                  MetaDescriptor (descriptorId unrelatedMetaDescriptor) infraDK
            isRelatedToUnrelated <-
              lift
                . hasSpecificRelation c
                $ unrelatedRelation
            when
              isRelatedToUnrelated
              (lift . deleteRelation c $ unrelatedRelation)
            hasAnyAdditionalRelations <- lift . isInfraToAny c $ infraDK
            unless
              hasAnyAdditionalRelations
              ( lift
                  . execute
                    c
                    "INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId) \
                    \VALUES (?,?)"
                  $ (metaDK, infraDK)
              )
        )
      return ()

isInfraToAny :: Connection -> DescriptorKey -> IO Bool
isInfraToAny c k = do
  r <-
    query
      c
      "SELECT * FROM MetaDescriptor \
      \WHERE infraDescriptorId = ?"
      [k] ::
      IO [Only Int]
  return . not . null $ r

hasSpecificRelation :: Connection -> MetaDescriptor -> IO Bool
hasSpecificRelation c (MetaDescriptor mdk idk) = do
  r <-
    query
      c
      "SELECT * FROM MetaDescriptor \
      \WHERE metaDescriptorId = ? AND infraDescriptorId = ?"
      (mdk, idk) ::
      IO [(Int, Int)]
  return . not . null $ r

deleteRelation :: Connection -> MetaDescriptor -> IO ()
deleteRelation c (MetaDescriptor mdk idk) =
  execute
    c
    "DELETE FROM MetaDescriptor \
    \metaDescriptorId = ? AND infraDescriptorId = ?"
    (mdk, idk)

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

fetchMetaDescriptors :: ToField a => Connection -> a -> IO [Descriptor]
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
  fmap concat . mapM (lookupFileWithTagByTagId c . fileId) $ fs

lookupFileWithTagByTagId :: Connection -> DescriptorKey -> IO [FileWithTags]
lookupFileWithTagByTagId conn t = do
  r <-
    query
      conn
      "SELECT f.id, f.filePath, d.id, d.descriptor \
      \FROM Tag t \
      \  JOIN File f \
      \    ON t.fileTagId = f.id \
      \  JOIN Descriptor d \
      \    ON t.descriptorTagId = d.id \
      \WHERE t.descriptorTagId = ?"
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
      \WHERE t.fileTagId = ?"
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

lookupFilePattern :: ToField a => Connection -> a -> IO [File]
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

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x