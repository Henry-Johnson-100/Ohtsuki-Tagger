{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.Task where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.Tagger.Access
import Database.Tagger.Type
import IO
import qualified Toml
import Type.Config
import Type.Model

type ConnString = String

exportConfig :: TaggerConfig -> IO ()
exportConfig tc = do
  configPath <- getConfigPath
  encodingMsg <- Toml.encodeToFile taggerConfigCodec configPath tc
  putStrLn . T.unpack $ encodingMsg

addPath :: Connection -> T.Text -> IO [FileWithTags]
addPath c p = do
  pathsToAdd <- getPathsToAdd p
  addedFiles <- mapM (addFile c) pathsToAdd
  return $ FileWithTags <$> addedFiles <*> []

tag :: Connection -> [FileWithTags] -> [T.Text] -> IO ()
tag c fwts dds = do
  withDescriptors <- fmap concat . mapM (lookupDescriptorPattern c) $ dds
  let newTags = Tag . (fileId . file) <$> fwts <*> map descriptorId withDescriptors
  mapM_ (newTag c) newTags

getRefreshedFWTs :: Connection -> [FileWithTags] -> IO [FileWithTags]
getRefreshedFWTs c fwts = do
  let fids = map (fileId . file) fwts
  refreshedFWTs <- mapM (lookupFileWithTagsByFileId c) fids
  return . concat $ refreshedFWTs

-- #TODO no assigned event
-- untagWith :: Connection -> [Tag] -> IO ()
-- untagWith = untag
untagWith :: Connection -> [FileWithTags] -> [T.Text] -> IO ()
untagWith c fwts dds = do
  let fids = map (fileId . file) fwts
  ds <- fmap (map descriptorId . concat) . mapM (lookupDescriptorPattern c) $ dds
  let tags = Tag <$> fids <*> ds
  untag c tags

relateTo :: Connection -> [Descriptor] -> [Descriptor] -> IO ()
relateTo c m i = do
  let metaDescriptors = MetaDescriptor <$> (descriptorId <$> m) <*> (descriptorId <$> i)
  _x <- runMaybeT . mapM_ (relate c) $ metaDescriptors
  return ()

unrelate :: Connection -> [Descriptor] -> IO ()
unrelate c i = do
  _x <- runMaybeT . mapM_ (Database.Tagger.Access.unrelate c . descriptorId) $ i
  return ()

getTagCounts :: Connection -> [Descriptor] -> IO [TagCount]
getTagCounts c ds = do
  mapM (getTagCount c) ds

queryByTag ::
  Connection ->
  [T.Text] ->
  IO [FileWithTags]
queryByTag c ts = do
  fwts <- mapM (lookupFileWithTagsByTagPattern c) ts
  return . concat $ fwts

queryUntagged ::
  Connection -> [T.Text] -> IO [FileWithTags]
queryUntagged c ns = do
  untaggedFwts <- getUntaggedFileWithTags c
  case ns of
    [] -> return untaggedFwts
    (n : _) ->
      if T.all isDigit n
        then return . take (read . T.unpack $ n) $ untaggedFwts
        else return untaggedFwts

queryRelation ::
  Connection -> [T.Text] -> IO [FileWithTags]
queryRelation c rs = do
  ds <- fmap concat . mapM (lookupDescriptorPattern c) $ rs
  fmap concat
    . mapM
      ( lookupFileWithTagsByRelation c
          . descriptorId
      )
    $ ds

queryFilePattern ::
  Connection -> [T.Text] -> IO [FileWithTags]
queryFilePattern c ps = do
  fwts <- mapM (lookupFileWithTagsByFilePattern c) ps
  return . concat $ fwts

doQueryWithCriteria ::
  QueryCriteria ->
  Connection ->
  [T.Text] ->
  IO [FileWithTags]
doQueryWithCriteria qc =
  case qc of
    ByTag -> queryByTag
    ByRelation -> queryRelation
    ByUntagged -> queryUntagged
    ByPattern -> queryFilePattern

lookupInfraDescriptorTree ::
  Connection -> T.Text -> IO DescriptorTree
lookupInfraDescriptorTree c dT = do
  result <-
    runMaybeT
      ( do
          d <-
            hoistMaybe . head'
              <=< lift . lookupDescriptorPattern c
              $ dT
          fetchInfraTree c . descriptorId $ d
      )
  return . fromMaybe NullTree $ result

getALLInfraTree ::
  Connection -> IO DescriptorTree
getALLInfraTree c = do
  result <-
    runMaybeT
      ( do
          allDescriptor <-
            hoistMaybe . head'
              <=< lift
                . lookupDescriptorPattern c
              $ "#ALL#"
          fetchInfraTree c . descriptorId $ allDescriptor
      )
  return . fromMaybe NullTree $ result

getUnrelatedInfraTree :: Connection -> IO DescriptorTree
getUnrelatedInfraTree c = do
  result <-
    runMaybeT
      ( do
          unrelatedDescriptor <-
            hoistMaybe . head' <=< lift . lookupDescriptorPattern c $ "#UNRELATED#"
          fetchInfraTree c . descriptorId $ unrelatedDescriptor
      )
  return . fromMaybe NullTree $ result

getParentDescriptorTree ::
  Connection ->
  DescriptorTree ->
  IO DescriptorTree
getParentDescriptorTree c tr = do
  result <-
    runMaybeT
      ( do
          headNode <- hoistMaybe . getNode $ tr
          firstMetaDescriptor <-
            hoistMaybe . head'
              <=< ( lift
                      . fetchMetaDescriptors c
                      . descriptorId
                  )
              $ headNode
          fetchInfraTree c
            . descriptorId
            $ firstMetaDescriptor
      )
  maybe (getALLInfraTree c) return result

createNewDescriptors :: Connection -> [T.Text] -> IO ()
createNewDescriptors c ts = do
  _x <- mapM (runMaybeT . addDescriptor c) ts
  return ()

deleteDescriptor :: Connection -> Descriptor -> IO ()
deleteDescriptor = Database.Tagger.Access.deleteDescriptor

-- const (pure . pure $ ()) . runMaybeT . Database.Tagger.Access.deleteDescriptor

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x