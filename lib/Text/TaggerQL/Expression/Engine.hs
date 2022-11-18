{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Text.TaggerQL.Expression.Engine (
  runQuery,

  -- * Primitive Functions
  runExpr,
  evalExpr,

  -- ** For Testing
  evalSubExpression,
  queryTags,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Functor ((<&>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Connection (query)
import Database.Tagger.Query (
  flatQueryForFileByTagDescriptorPattern,
  flatQueryForFileOnMetaRelationPattern,
  queryForFileByPattern,
  queryForUntaggedFiles,
 )
import Database.Tagger.Type (
  File,
  Tag (tagId, tagSubtagOfId),
  TaggedConnection,
 )
import Text.Parsec.Error (errorMessages, messageString)
import Text.RawString.QQ (r)
import Text.TaggerQL.Expression.AST (
  Expression (..),
  FileTerm (FileTerm),
  SubExpression (..),
  TagTerm (..),
 )
import Text.TaggerQL.Expression.Parser (parseExpr)

{- |
 Run a TaggerQL query on the given database.
-}
runQuery :: TaggedConnection -> Text -> Either [Text] (IO (HashSet File))
runQuery c t =
  let result = parseExpr t
   in case result of
        Left pe -> Left . map (T.pack . messageString) . errorMessages $ pe
        Right ex -> Right . flip runExpr c $ ex

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
  TagTermValue tt ->
    ask
      >>= liftIO . fmap HS.fromList . case tt of
        DescriptorTerm txt -> flatQueryForFileByTagDescriptorPattern txt
        MetaDescriptorTerm txt -> flatQueryForFileOnMetaRelationPattern txt
  TagExpression tt subExpr ->
    ask >>= \c -> do
      supertags <- liftIO . fmap HS.fromList $ queryTags tt c
      subExprResult <- evalSubExpression subExpr supertags
      liftIO $ toFileSet subExprResult c
  Binary lhs so rhs ->
    ( case so of
        Union -> HS.union
        Intersect -> HS.intersection
        Difference -> HS.difference
    )
      <$> evalExpr lhs
      <*> evalExpr rhs

evalSubExpression ::
  SubExpression ->
  HashSet Tag ->
  ReaderT
    TaggedConnection
    IO
    (HashSet Tag)
evalSubExpression subExpr supertags = case subExpr of
  SubTag tt -> do
    c <- ask
    subtags <- liftIO . fmap (HS.fromList . map tagSubtagOfId) $ queryTags tt c
    return $ joinSubtags subtags
  SubExpression tt se -> do
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
  SubBinary se so se' ->
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
