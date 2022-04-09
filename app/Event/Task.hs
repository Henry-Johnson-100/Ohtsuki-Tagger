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
import qualified Database.Tagger.Access as TaggerNew.Access
import qualified Database.Tagger.Type as TaggerNew.Type
import Type.Model

type ConnString = String

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

queryByTagNew ::
  TaggerNew.Access.Connection ->
  [T.Text] ->
  IO [TaggerNew.Type.FileWithTags]
queryByTagNew c ts = do
  fwts <- mapM (TaggerNew.Access.lookupFileWithTagsByTagPattern c) ts
  return . concat $ fwts

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