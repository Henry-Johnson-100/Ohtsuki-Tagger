{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.Expression.Engine (
  runQuery,

  -- * Primitive Functions
  runExpr,
  evalExpr,

  -- ** For Testing
  runSubExprOnFile,
  evalSubExpression,
  queryTags,
) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Data.Functor ((<&>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (RecordKey, insertTags, queryForDescriptorByPattern, queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf)
import Database.Tagger.Connection (query)
import Database.Tagger.Query (flatQueryForFileByTagDescriptorPattern, flatQueryForFileOnMetaRelationPattern, queryForFileByPattern, queryForTagBySubTagTriple, queryForUntaggedFiles)
import Database.Tagger.Type (
  File,
  Tag (tagId, tagSubtagOfId),
  TaggedConnection,
  descriptorId,
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
runQuery :: TaggedConnection -> Text -> ExceptT [Text] IO (HashSet File)
runQuery c t =
  let result = parseExpr t
   in case result of
        Left pe -> throwE . map (T.pack . messageString) . errorMessages $ pe
        Right ex -> liftIO . flip runExpr c $ ex

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

-- Tagging Engine

runSubExprOnFile :: SubExpression -> RecordKey File -> TaggedConnection -> IO ()
runSubExprOnFile se fk c = void (runReaderT (insertSubExpr se Nothing) (fk, c))

insertSubExpr ::
  SubExpression ->
  Maybe [RecordKey Tag] ->
  ReaderT (RecordKey File, TaggedConnection) IO [RecordKey Tag]
insertSubExpr se supertags =
  asks snd >>= \c ->
    ( case se of
        SubTag tt -> do
          let txt = termTxt tt
          withDescriptors <- qDescriptor txt
          fk <- asks fst
          let tagTriples =
                (fk,,)
                  <$> (descriptorId <$> withDescriptors)
                    <*> maybe [Nothing] (fmap Just) supertags
          void . liftIO . insertTags tagTriples $ c
          -- Tag insertion may fail because some tags of the same form already exist.
          -- This query gets all of those pre-existing tags,
          -- and returns them as if they were just made.
          case supertags of
            -- If nothing, then these are top-level tags
            Nothing ->
              map tagId
                <$> liftIO
                  ( queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf
                      fk
                      txt
                      c
                  )
            Just _ ->
              -- If just, these are subtags of existing tags.
              map tagId . unions
                <$> liftIO
                  ( mapM
                      (`queryForTagBySubTagTriple` c)
                      (third fromJust <$> tagTriples)
                  )
        SubExpression tt se' -> do
          insertedSubtags <- insertSubExpr (SubTag tt) supertags
          insertSubExpr se' (Just insertedSubtags)
        SubBinary se' _ se2 -> do
          void $ insertSubExpr se' supertags
          void $ insertSubExpr se2 supertags
          -- tags inserted by a SubBinary is indeterminate and empty by default
          return mempty
    )
 where
  termTxt tt =
    case tt of
      DescriptorTerm txt -> txt
      MetaDescriptorTerm txt -> txt
  qDescriptor txt = asks snd >>= liftIO . queryForDescriptorByPattern txt
  unions :: (Foldable t, Eq a) => t [a] -> [a]
  unions xs = if null xs then [] else L.foldl' L.union [] xs
  third f (x, y, z) = (x, y, f z)