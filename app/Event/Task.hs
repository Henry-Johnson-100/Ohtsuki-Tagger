{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.Task where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Database.Tagger.Access
import Database.Tagger.Type
import qualified Database.TaggerNew.Access as TaggerNew.Access
import qualified Database.TaggerNew.Type as TaggerNew.Type
import Type.Model

type ConnString = String

tagSelection :: ConnString -> [FileWithTags] -> [String] -> IO ()
tagSelection cs fwts ds = connectThenRun cs $ do
  faks <- fmap catMaybes . mapM (lookupFileAutoKey . filePath . file) $ fwts
  daks <- fmap catMaybes . mapM lookupDescriptorAutoKey $ ds
  mapM_ tag $ Tag <$> faks <*> daks

tagThenGetRefresh :: ConnString -> [FileWithTags] -> [String] -> IO [FileWithTags]
tagThenGetRefresh cs fwts ds = connectThenRun cs $ do
  faks <- fmap catMaybes . mapM (lookupFileAutoKey . filePath . file) $ fwts
  daks <- fmap catMaybes . mapM lookupDescriptorAutoKey $ ds
  mapM_ tag $ Tag <$> faks <*> daks
  refreshedFwts <- fmap catMaybes . mapM getFileWithTags $ faks
  liftIO . return $ refreshedFwts

tagThenGetRefreshNew ::
  TaggerNew.Access.Connection ->
  [TaggerNew.Type.FileWithTags] ->
  [T.Text] ->
  IO [TaggerNew.Type.FileWithTags]
tagThenGetRefreshNew c fwts dds = do
  tagWithDescriptors <-
    fmap concat . mapM (TaggerNew.Access.lookupDescriptorPattern c) $ dds
  let newTags =
        TaggerNew.Type.Tag
          <$> map (TaggerNew.Type.fileId . TaggerNew.Type.file) fwts
            <*> map TaggerNew.Type.descriptorId tagWithDescriptors
  mapM_ (TaggerNew.Access.newTag c) newTags
  newFwts <-
    mapM
      ( TaggerNew.Access.lookupFileWithTagsByFileId c
          . (TaggerNew.Type.fileId . TaggerNew.Type.file)
      )
      fwts
  return . concat $ newFwts

queryByTag :: ConnString -> [String] -> IO [FileWithTags]
queryByTag cs ts =
  connectThenRun cs $ do
    tagQResult <- mapM lookupDescriptorAutoKey ts
    fileAKsWithTags <-
      fmap (Data.List.foldl1' union)
        . mapM fetchFilesWithTag
        . catMaybes
        $ tagQResult
    filesWithTags <- fmap catMaybes . mapM getFileWithTags $ fileAKsWithTags
    liftIO . return $ filesWithTags

queryByTagNew ::
  TaggerNew.Access.Connection ->
  [T.Text] ->
  IO [TaggerNew.Type.FileWithTags]
queryByTagNew c ts = do
  fwts <- mapM (TaggerNew.Access.lookupFileWithTagsByTagPattern c) ts
  return . concat $ fwts

queryUntagged :: ConnString -> [String] -> IO [FileWithTags]
queryUntagged cs ns =
  connectThenRun cs $ do
    untaggedKeys <-
      case ns of
        [] -> fetchUntaggedFiles
        (n : _) ->
          if Data.List.all isDigit n
            then fmap (take . read $ n) fetchUntaggedFiles
            else fetchUntaggedFiles
    filesWithTags <- fmap catMaybes . mapM getFileWithTags $ untaggedKeys
    liftIO . return $ filesWithTags

queryUntaggedNew ::
  TaggerNew.Access.Connection -> [T.Text] -> IO [TaggerNew.Type.FileWithTags]
queryUntaggedNew c ns = do
  untaggedFwts <- TaggerNew.Access.getUntaggedFileWithTags c
  case ns of
    [] -> return untaggedFwts
    (n : _) ->
      if T.all isDigit n
        then return . take (read . T.unpack $ n) $ untaggedFwts
        else return untaggedFwts

-- | Kinda hacky
queryRelation :: ConnString -> [String] -> IO [FileWithTags]
queryRelation cs ds =
  connectThenRun cs $ do
    tagQResult <- fmap catMaybes . mapM lookupDescriptorAutoKey $ ds
    infraTrees <-
      fmap (Data.List.concatMap flattenTree) . mapM fetchInfraTree $ tagQResult
    fwts <- liftIO . queryByTag cs . map descriptor $ infraTrees
    liftIO . return $ fwts

queryRelationNew ::
  TaggerNew.Access.Connection -> [T.Text] -> IO [TaggerNew.Type.FileWithTags]
queryRelationNew c rs = do
  ds <- fmap concat . mapM (TaggerNew.Access.lookupDescriptorPattern c) $ rs
  fmap concat
    . mapM
      ( TaggerNew.Access.lookupFileWithTagsByRelation c
          . TaggerNew.Type.descriptorId
      )
    $ ds

doQueryWithCriteria :: QueryCriteria -> ConnString -> [String] -> IO [FileWithTags]
doQueryWithCriteria qc =
  case qc of
    ByTag -> queryByTag
    ByRelation -> queryRelation
    ByUntagged -> queryUntagged

doQueryWithCriteriaNew ::
  QueryCriteria ->
  TaggerNew.Access.Connection ->
  [T.Text] ->
  IO [TaggerNew.Type.FileWithTags]
doQueryWithCriteriaNew qc =
  case qc of
    ByTag -> queryByTagNew
    ByRelation -> queryRelationNew
    ByUntagged -> queryUntaggedNew

lookupDescriptorTreeDir ::
  ConnString ->
  (DescriptorAutoKey -> Result DescriptorTree) ->
  String ->
  IO DescriptorTree
lookupDescriptorTreeDir cs d lk = connectThenRun cs $ do
  mk <- lookupDescriptorAutoKey lk
  ktr <- maybe (return NullTree) d mk
  liftIO . return $ ktr

lookupInfraDescriptorTree :: ConnString -> String -> IO DescriptorTree
lookupInfraDescriptorTree cs = lookupDescriptorTreeDir cs fetchInfraTree

lookupInfraDescriptorTreeNew ::
  TaggerNew.Access.Connection -> T.Text -> IO TaggerNew.Type.DescriptorTree
lookupInfraDescriptorTreeNew c dT = do
  result <-
    runMaybeT
      ( do
          d <-
            TaggerNew.Access.hoistMaybe . head'
              <=< lift . TaggerNew.Access.lookupDescriptorPattern c
              $ dT
          TaggerNew.Access.fetchInfraTree c . TaggerNew.Type.descriptorId $ d
      )
  return . fromMaybe TaggerNew.Type.NullTree $ result

getParentDescriptorTree :: ConnString -> DescriptorTree -> IO DescriptorTree
getParentDescriptorTree cs dtr =
  connectThenRun cs $ do
    headNode <- return . getNode $ dtr
    mk <- maybe (return Nothing) (lookupDescriptorAutoKey . descriptor) headNode
    metaTreeChildren <- maybe (return []) (fmap descriptorTreeChildren . fetchMetaTree) mk
    ch <- return . (\xs -> case xs of [] -> Nothing; x : _ -> Just x) $ metaTreeChildren
    liftIO $ maybe (lookupInfraDescriptorTree cs "#ALL#") (liftIO . return) ch

getALLInfraTree ::
  TaggerNew.Access.Connection -> IO TaggerNew.Type.DescriptorTree
getALLInfraTree c = do
  result <-
    runMaybeT
      ( do
          allDescriptor <-
            TaggerNew.Access.hoistMaybe . head'
              <=< lift
                . TaggerNew.Access.lookupDescriptorPattern c
              $ "#ALL#"
          TaggerNew.Access.fetchInfraTree c . TaggerNew.Type.descriptorId $ allDescriptor
      )
  return . fromMaybe TaggerNew.Type.NullTree $ result

getParentDescriptorTreeNew ::
  TaggerNew.Access.Connection ->
  TaggerNew.Type.DescriptorTree ->
  IO TaggerNew.Type.DescriptorTree
getParentDescriptorTreeNew c tr = do
  result <-
    runMaybeT
      ( do
          headNode <- TaggerNew.Access.hoistMaybe . TaggerNew.Type.getNode $ tr
          firstMetaDescriptor <-
            TaggerNew.Access.hoistMaybe . head'
              <=< ( lift
                      . TaggerNew.Access.fetchMetaDescriptors c
                      . TaggerNew.Type.descriptorId
                  )
              $ headNode
          TaggerNew.Access.fetchInfraTree c
            . TaggerNew.Type.descriptorId
            $ firstMetaDescriptor
      )
  maybe (getALLInfraTree c) return result

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x