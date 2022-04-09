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
import Type.Model

type ConnString = String

tagThenGetRefreshNew ::
  Connection ->
  [FileWithTags] ->
  [T.Text] ->
  IO [FileWithTags]
tagThenGetRefreshNew c fwts dds = do
  tagWithDescriptors <-
    fmap concat . mapM (lookupDescriptorPattern c) $ dds
  let newTags =
        Tag
          <$> map (fileId . file) fwts
            <*> map descriptorId tagWithDescriptors
  mapM_ (newTag c) newTags
  newFwts <-
    mapM
      ( lookupFileWithTagsByFileId c
          . (fileId . file)
      )
      fwts
  return . concat $ newFwts

queryByTagNew ::
  Connection ->
  [T.Text] ->
  IO [FileWithTags]
queryByTagNew c ts = do
  fwts <- mapM (lookupFileWithTagsByTagPattern c) ts
  return . concat $ fwts

queryUntaggedNew ::
  Connection -> [T.Text] -> IO [FileWithTags]
queryUntaggedNew c ns = do
  untaggedFwts <- getUntaggedFileWithTags c
  case ns of
    [] -> return untaggedFwts
    (n : _) ->
      if T.all isDigit n
        then return . take (read . T.unpack $ n) $ untaggedFwts
        else return untaggedFwts

queryRelationNew ::
  Connection -> [T.Text] -> IO [FileWithTags]
queryRelationNew c rs = do
  ds <- fmap concat . mapM (lookupDescriptorPattern c) $ rs
  fmap concat
    . mapM
      ( lookupFileWithTagsByRelation c
          . descriptorId
      )
    $ ds

doQueryWithCriteriaNew ::
  QueryCriteria ->
  Connection ->
  [T.Text] ->
  IO [FileWithTags]
doQueryWithCriteriaNew qc =
  case qc of
    ByTag -> queryByTagNew
    ByRelation -> queryRelationNew
    ByUntagged -> queryUntaggedNew

lookupInfraDescriptorTreeNew ::
  Connection -> T.Text -> IO DescriptorTree
lookupInfraDescriptorTreeNew c dT = do
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

getParentDescriptorTreeNew ::
  Connection ->
  DescriptorTree ->
  IO DescriptorTree
getParentDescriptorTreeNew c tr = do
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

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x