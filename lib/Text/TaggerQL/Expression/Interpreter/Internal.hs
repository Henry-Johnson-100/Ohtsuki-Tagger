{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Expression.Interpreter.Internal (
  toFileSet,
  evalSubExpression,
  queryTags,
  joinSubTags',
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Tagger (SetOp (..))
import Database.Tagger (
  File,
  Tag (tagId, tagSubtagOfId),
  TaggedConnection,
  query,
 )
import Text.RawString.QQ (r)
import Text.TaggerQL.Expression.AST

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
      WHERE t.id = ?|]

{- |
 Given a 'SubExpression` and a set of 'Tag`, compute the set of 'Tag` that
 is defined by the 'SubExpression` and filter the argument by those that are subtags.
-}
evalSubExpression ::
  SubExpression Identity ->
  -- | The current 'Tag` environment. Any set of 'Tag` computed by this function
  -- will be a subset of this argument.
  HashSet Tag ->
  ReaderT
    TaggedConnection
    IO
    (HashSet Tag)
evalSubExpression subExpr supertags = case subExpr of
  SubTag (Identity tt) -> do
    c <- ask
    subtags <- liftIO . fmap (HS.fromList . map tagSubtagOfId) $ queryTags tt c
    return $ joinSubtags subtags
  SubExpression (Identity (SubExpressionExtension tt se)) -> do
    c <- ask
    nextTagEnv <- liftIO . fmap HS.fromList $ queryTags tt c
    subExprResult <- fmap (HS.map tagSubtagOfId) . evalSubExpression se $ nextTagEnv
    return $ joinSubtags subExprResult
  {-
  For a given set of supertags, evalSubExpression.joinSubtags is closed on that set.
  Meaning that any given set of tags returned by evalSubExpression will be a subset of
    the set it was given.

  Therefore, the SubBinary case does not need a final intersection of its
  product with supertags, because both operands are subsets of the supertag set.
  -}
  SubBinary (Identity (BinarySubExpression se so se')) ->
    ( evalSubExpression se supertags
        <&> ( case so of
                Union -> HS.union
                Intersect -> HS.intersection
                Difference -> HS.difference
            )
    )
      <*> evalSubExpression se' supertags
 where
  -- Filter the given set of tags based on whether or not it appears in the latter given
  -- set of subTagOfIds.
  joinSubtags subtags =
    HS.filter (\(Just . tagId -> supertagId) -> HS.member supertagId subtags) supertags

joinSubTags' supertags subtags =
  HS.filter (\(Just . tagId -> supertagId) -> HS.member supertagId subtags) supertags

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
