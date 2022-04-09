{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.TaggerNew.Access () where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.TaggerNew.Type

fetchMetaTree :: Connection -> Int -> MaybeT IO DescriptorTree
fetchMetaTree c pid = do
  did <- getDescriptor c pid
  let parentTree = Infra did
  childrenTrees <-
    (lift . fetchMetaDescriptors c >=> mapM (fetchMetaTree c . descriptorId)) pid
  return . foldl' insertIntoDescriptorTree parentTree $ childrenTrees

fetchInfraTree :: Connection -> Int -> MaybeT IO DescriptorTree
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

fetchInfraDescriptors :: Connection -> Int -> IO [Descriptor]
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

getDescriptor :: Connection -> Int -> MaybeT IO Descriptor
getDescriptor c did = do
  r <-
    lift $
      query
        c
        "SELECT id , descriptor FROM Descriptor WHERE id = ?"
        [did]
  hoistMaybe . fmap mapQToDescriptor . head' $ r

getFile :: Connection -> Int -> MaybeT IO File
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

lookupFileWithTagByTag :: (ToField a) => Connection -> a -> IO [FileWithTags]
lookupFileWithTagByTag conn t = do
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

lookupDescriptorPattern :: (ToField a) => Connection -> a -> IO [Descriptor]
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
      \FROM File\
      \ WHERE filePath LIKE ?"
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