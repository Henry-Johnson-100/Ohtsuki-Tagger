{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Module      : Text.TaggerQL.Expression.Engine
Description : The interpreter for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Contains functions that interprets the TaggerQL query language to either run queries
  or tag a file with a certain expression.
-}
module Text.TaggerQL.Expression.Engine (
  runQuery,
  tagFile,

  -- * Interpreters
  ExpressionInterpreter (..),
  runExpressionInterpreter,
  SubExpressionInterpreter (..),
  runSubExpressionInterpreter,

  -- ** Examples
  queryer,

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
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Data.Functor (($>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  RecordKey,
  insertTags,
  queryForDescriptorByPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
 )
import Database.Tagger.Connection (query)
import Database.Tagger.Query (
  flatQueryForFileByTagDescriptorPattern,
  flatQueryForFileOnMetaRelationPattern,
  queryForFileByPattern,
  queryForTagBySubTagTriple,
 )
import Database.Tagger.Type (
  File,
  Tag (tagId, tagSubtagOfId),
  TaggedConnection,
  descriptorId,
 )
import Text.Parsec.Error (errorMessages, messageString)
import Text.RawString.QQ (r)
import Text.TaggerQL.Expression.AST (
  BinaryOperation (BinaryOperation),
  Expression (..),
  FileTerm (FileTerm),
  SubExpression (..),
  TagTerm (..),
  TagTermExtension (TagTermExtension),
 )
import Text.TaggerQL.Expression.Parser (parseExpr, parseTagExpr)

{- |
 Defines a computation for each constructor of a 'SubExpression`.

 Used to monadically fold a 'SubExpression`.
-}
data SubExpressionInterpreter n b = SubExpressionInterpreter
  { interpretSubTag :: TagTerm -> n b
  , interpretBinarySubExpression :: BinaryOperation b -> n b
  , interpretSubExpression :: TagTermExtension (n b) -> n b
  }

{- |
 Run the given 'SubExpressionInterpreter` over a 'SubExpression`.
-}
runSubExpressionInterpreter ::
  Monad n =>
  SubExpressionInterpreter n b ->
  SubExpression ->
  n b
runSubExpressionInterpreter setr@(SubExpressionInterpreter a b c) se = case se of
  SubTag tt -> a tt
  BinarySubExpression (BinaryOperation lhs so rhs) -> do
    lhsR <- runSubExpressionInterpreter setr lhs
    rhsR <- runSubExpressionInterpreter setr rhs
    b $ BinaryOperation lhsR so rhsR
  SubExpression (TagTermExtension tt se') ->
    c
      . TagTermExtension tt
      . runSubExpressionInterpreter setr
      $ se'

{- |
 Defines a computation for each constructor of an 'Expression`.

 Used to monadically fold an 'Expression`.
-}
data ExpressionInterpreter m b a = ExpressionInterpreter
  { interpretFileTerm :: FileTerm -> m a
  , interpretTagTerm :: TagTerm -> m a
  , interpretTagExpression :: TagTermExtension (m b) -> m a
  , interpretBinaryExpression :: BinaryOperation a -> m a
  , subExpressionInterpreter :: SubExpressionInterpreter m b
  }

{- |
 Run the given 'ExpressionInterpreter` over an 'Expression`. Appropriately
 sequencing monadic actions and computational side effects when extending the
 'Expression` with a 'SubExpression`.
-}
runExpressionInterpreter :: Monad m => ExpressionInterpreter m b a -> Expression -> m a
runExpressionInterpreter itr@(ExpressionInterpreter ift itt itte ibo setr) expr =
  case expr of
    FileTermValue ft -> ift ft
    TagTermValue tt -> itt tt
    TagExpression (TagTermExtension tt se) ->
      itte
        . TagTermExtension tt
        . runSubExpressionInterpreter setr
        $ se
    BinaryExpression (BinaryOperation lhs so rhs) -> do
      lhsR <- runExpressionInterpreter itr lhs
      rhsR <- runExpressionInterpreter itr rhs
      ibo $ BinaryOperation lhsR so rhsR

{- |
 Defines an 'Interpreter` that queries the given connection for a set of 'File`.

 The 'SubExpression` evaluates to a function accepting a set of 'Tag` that are defined
 by the outer scope of any given 'SubExpression`. Meaning that a set of 'Tag`
 is created from the bottom-up of a given tag-subtag hierarchy.
-}
queryer ::
  ExpressionInterpreter
    (ReaderT TaggedConnection IO)
    (HashSet Tag -> HashSet Tag)
    (HashSet File)
queryer =
  ExpressionInterpreter
    { interpretFileTerm = \(FileTerm t) ->
        ask
          >>= liftIO
            . fmap HS.fromList
            . queryForFileByPattern t
    , interpretTagTerm = \tt ->
        ask
          >>= liftIO
            . fmap HS.fromList
            . ( case tt of
                  DescriptorTerm txt -> flatQueryForFileByTagDescriptorPattern txt
                  MetaDescriptorTerm txt -> flatQueryForFileOnMetaRelationPattern txt
              )
    , interpretBinaryExpression = \(BinaryOperation lhs so rhs) ->
        return $ hsOp so lhs rhs
    , interpretTagExpression = \(TagTermExtension tt se) -> do
        c <- ask
        supertags <- liftIO . fmap HS.fromList $ queryTags tt c
        joinSubTagsTo <- se
        liftIO $ toFileSet (joinSubTagsTo supertags) c
    , subExpressionInterpreter =
        SubExpressionInterpreter
          { interpretSubTag = \tt ->
              flip joinSubtags
                <$> ( ask
                        >>= liftIO
                          . fmap (HS.fromList . map tagSubtagOfId)
                          . queryTags tt
                    )
          , interpretBinarySubExpression = \(BinaryOperation lhs so rhs) ->
              return $ \higherEnv -> hsOp so (lhs higherEnv) (rhs higherEnv)
          , interpretSubExpression = \(TagTermExtension tt se) -> do
              c <- ask
              supertags <- liftIO . fmap HS.fromList $ queryTags tt c
              joinLowerEnv <- se
              return $ \higherEnv ->
                joinSubtags
                  higherEnv
                  (HS.map tagSubtagOfId $ joinLowerEnv supertags)
          }
    }
 where
  joinSubtags supertags subtags =
    HS.filter (\(Just . tagId -> supertagId) -> HS.member supertagId subtags) supertags
  hsOp so =
    case so of
      Union -> HS.union
      Intersect -> HS.intersection
      Difference -> HS.difference

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
-}
runExpr :: Expression -> TaggedConnection -> IO (HashSet File)
runExpr expr = runReaderT (evalExpr expr)

evalExpr :: Expression -> ReaderT TaggedConnection IO (HashSet File)
evalExpr = runExpressionInterpreter queryer

{- |
 Given a 'SubExpression` and a set of 'Tag`, compute the set of 'Tag` that
 is defined by the 'SubExpression` and filter the argument by those that are subtags.
-}
evalSubExpression ::
  SubExpression ->
  -- | The current 'Tag` environment. Any set of 'Tag` computed by this function
  -- will be a subset of this argument.
  HashSet Tag ->
  ReaderT
    TaggedConnection
    IO
    (HashSet Tag)
evalSubExpression subExpr supertags = do
  joinSubTagsTo <- runSubExpressionInterpreter (subExpressionInterpreter queryer) subExpr
  return . joinSubTagsTo $ supertags

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

{- |
 Run a sub-expression, a subset of the TaggerQL, to tag a file with Descriptors
 matching the given patterns.

 Returns Just error messages if parsing fails. Otherwise Nothing.
-}
tagFile :: RecordKey File -> TaggedConnection -> Text -> IO (Maybe Text)
tagFile fk c =
  either
    (return . Just . T.pack . show)
    (\se -> runSubExprOnFile se fk c $> Nothing)
    . parseTagExpr

runSubExprOnFile :: SubExpression -> RecordKey File -> TaggedConnection -> IO ()
runSubExprOnFile se fk c = void (runReaderT (insertSubExpr se Nothing) (fk, c))

insertSubExpr ::
  SubExpression ->
  Maybe [RecordKey Tag] ->
  ReaderT (RecordKey File, TaggedConnection) IO [RecordKey Tag]
insertSubExpr se supertags =
  asks snd >>= \c ->
    ( case se of
        SubExpression (TagTermExtension tt se') -> do
          insertedSubtags <- insertSubExpr (SubTag tt) supertags
          insertSubExpr se' (Just insertedSubtags)
        BinarySubExpression (BinaryOperation se' _ se2) -> do
          void $ insertSubExpr se' supertags
          void $ insertSubExpr se2 supertags
          -- tags inserted by a SubBinary is indeterminate and empty by default
          return mempty
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