{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Event.Task where

import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Database.Tagger.Access
import Database.Tagger.Type
import Type.Model

type ConnString = String

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

-- | Kinda hacky
queryRelation :: ConnString -> [String] -> IO [FileWithTags]
queryRelation cs ds =
  connectThenRun cs $ do
    tagQResult <- fmap catMaybes . mapM lookupDescriptorAutoKey $ ds
    infraTrees <-
      fmap (Data.List.concatMap flattenTree) . mapM fetchInfraTree $ tagQResult
    fwts <- liftIO . queryByTag cs . map descriptor $ infraTrees
    liftIO . return $ fwts

doQueryWithCriteria :: QueryCriteria -> ConnString -> [String] -> IO [FileWithTags]
doQueryWithCriteria qc =
  case qc of
    ByTag -> queryByTag
    ByRelation -> queryRelation
    ByUntagged -> queryUntagged

getAllFilesIO :: ConnString -> IO [FileWithTags]
getAllFilesIO =
  flip
    connectThenRun
    (fetchAllFiles >>= fmap catMaybes . mapM getFileWithTags >>= liftIO . return)

getAllIndexedDescriptorsIO :: ConnString -> IO [Descriptor]
getAllIndexedDescriptorsIO =
  flip
    connectThenRun
    ( fetchAllIndexedDescriptors
        >>= liftIO . return
    )

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

getParentDescriptorTree :: ConnString -> DescriptorTree -> IO DescriptorTree
getParentDescriptorTree cs dtr =
  connectThenRun cs $ do
    headNode <- return . getNode $ dtr
    mk <- maybe (return Nothing) (lookupDescriptorAutoKey . descriptor) headNode
    metaTreeChildren <- maybe (return []) (fmap descriptorTreeChildren . fetchMetaTree) mk
    ch <- return . (\xs -> case xs of [] -> Nothing; x : _ -> Just x) $ metaTreeChildren
    liftIO $ maybe (lookupInfraDescriptorTree cs "#ALL#") (liftIO . return) ch