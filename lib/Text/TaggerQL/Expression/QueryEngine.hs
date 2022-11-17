{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Text.TaggerQL.Expression.QueryEngine (
  runExpr,
  evalExpr,
  evalTagExpr,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Tagger (SetOp (..))
import Database.Tagger.Connection (query)
import Database.Tagger.Query (
  queryForFileByPattern,
  queryForUntaggedFiles,
 )
import Database.Tagger.Type (
  File,
  Tag (tagId, tagSubtagOfId),
  TaggedConnection,
 )
import Text.RawString.QQ (r)
import Text.TaggerQL.Expression.AST (
  Expression (..),
  FileTerm (FileTerm),
  TagExpression (..),
  TagTerm (..),
 )

{- |
 Query an 'Expression`

 thin wrapper for 'evalExpr`
-}
runExpr :: Expression -> TaggedConnection -> IO (HashSet File)
runExpr expr = runReaderT (evalExpr expr)

evalExpr :: Expression -> ReaderT TaggedConnection IO (HashSet File)
evalExpr expr = case expr of
  UntaggedConst -> ask >>= liftIO . fmap HS.fromList . queryForUntaggedFiles
  FileTermValue (FileTerm txt) ->
    ask >>= liftIO . fmap HS.fromList . queryForFileByPattern txt
  TagExpression te -> do
    tagResults <- evalTagExpr te
    ask >>= liftIO . toFileSet tagResults
  Binary ex so ex' -> do
    lhs <- evalExpr ex
    rhs <- evalExpr ex'
    return $ dispatchComb so lhs rhs

-- there's no way this is right.
--
-- It's so much simpler than the last query engine that it has to be wrong.
--
-- Returns a set of supertags that is rolled up through higher recursions.
-- Instead of recursing down through an expression, we pretend that we work our way
-- bottom-up. From this assumption, it is clear that the way we search via subtags
-- is by filtering the set of supertags by membership in a set of subtags.
-- This way, all returned tag sets are a subset of the original tagset returned
-- by the TagTerm tt
evalTagExpr ::
  TagExpression -> ReaderT TaggedConnection IO (HashSet Tag)
evalTagExpr texpr = case texpr of
  TagValue tt -> ask >>= liftIO . fmap HS.fromList . queryTags tt
  TagDistribution tt te -> do
    supertags <- ask >>= liftIO . fmap HS.fromList . queryTags tt
    !subtags <- HS.map tagSubtagOfId <$> evalTagExpr te
    return
      . HS.filter
        (\supertag -> HS.member (Just . tagId $ supertag) subtags)
      $ supertags
  TagBinaryDistribution tt ex so ex' -> do
    supertags <- ask >>= liftIO . fmap HS.fromList . queryTags tt
    !lhs <- HS.map tagSubtagOfId <$> evalTagExpr ex
    !rhs <- HS.map tagSubtagOfId <$> evalTagExpr ex'
    let filterCriteria x y = case so of
          Union -> x || y
          Intersect -> x && y
          Difference -> x && not y
    return
      . HS.filter
        ( \st ->
            let superId = Just . tagId $ st
             in filterCriteria (HS.member superId lhs) (HS.member superId rhs)
        )
      $ supertags

dispatchComb :: Hashable a => SetOp -> HashSet a -> HashSet a -> HashSet a
dispatchComb so =
  case so of
    Union -> HS.union
    Intersect -> HS.intersection
    Difference -> HS.difference

toFileSet :: HashSet Tag -> TaggedConnection -> IO (HashSet File)
toFileSet (map tagId . HS.toList -> ts) conn = do
  results <- mapM (query conn q . (: [])) ts
  return . HS.unions . map HS.fromList $ results
 where
  q =
    [r|
SELECT
  f.id
  ,f.filePath
FROM File f
JOIN Tag t ON f.id = t.fileId
WHERE t.id = ?
    |]

queryTags :: TagTerm -> TaggedConnection -> IO [Tag]
queryTags tt c =
  case tt of
    DescriptorTerm txt -> query c tagQueryOnDescriptorPattern [txt]
    MetaDescriptorTerm txt -> query c tagQueryOnMetaDescriptorPattern [txt]
 where
  tagQueryOnDescriptorPattern =
    [r|
      SELECT
        t.id,
        t.fileId,
        t.descriptorId,
        t.subTagOfId
      FROM Tag t
      JOIN Descriptor d ON t.descriptorId = d.id
      WHERE d.descriptor LIKE ? ESCAPE '\'
      |]
  tagQueryOnMetaDescriptorPattern =
    [r|
      SELECT
        t.id
        ,t.fileId
        ,t.descriptorId
        ,t.subTagOfId
      FROM Tag t
      JOIN (
        WITH RECURSIVE r(id) AS (
          SELECT id
          FROM Descriptor
          WHERE descriptor LIKE ? ESCAPE '\'
          UNION
          SELECT infraDescriptorId
          FROM MetaDescriptor md
          JOIN r ON md.metaDescriptorId = r.id
        )
        SELECT id FROM r
      ) AS d ON t.descriptorId = d.id|]
