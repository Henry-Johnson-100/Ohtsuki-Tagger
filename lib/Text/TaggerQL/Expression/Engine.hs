{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Move guards forward" #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

  -- * Indexing
  ExpressionIndex (..),
  exprAt,
  foldIxGen,
  flatten,
  index,
  indexWith,

  -- * New
  queryQueryExpression,
  insertTagExpression,
) where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (foldM, guard, void, when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans.State.Strict (State, StateT, evalState, execState, get, gets, modify, runState)
import Data.Bifunctor (Bifunctor (first, second))
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
  allFiles,
  allTags,
  insertTags,
  queryForDescriptorByPattern,
  queryForFileByTagId,
  queryForTagByDescriptorPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagByMetaDescriptorPattern,
 )
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
import Lens.Micro (Lens', lens, (%~), (&), (.~), (^.))
import Text.Parsec.Error (errorMessages, messageString)
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Parser (parseQueryExpression, parseTagExpression)
import Prelude hiding (lookup, (!!))

joinSubtags :: HashSet Tag -> HashSet (Maybe (RecordKey Tag)) -> HashSet Tag
joinSubtags supertags subtags =
  HS.filter (\(Just . tagId -> supertagId) -> HS.member supertagId subtags) supertags

{- |
 Run a TaggerQL query on the given database.
-}
runQuery :: TaggedConnection -> Text -> ExceptT [Text] IO (HashSet File)
runQuery c =
  either
    (throwE . map (T.pack . messageString) . errorMessages)
    (liftIO . queryQueryExpression c)
    . parseQueryExpression

{- |
 Tag a file with the given 'TagExpression`
-}
tagFile :: RecordKey File -> TaggedConnection -> Text -> IO (Maybe Text)
tagFile fk c =
  either
    (pure . Just . T.pack . show)
    (fmap (const Nothing) . insertTagExpression c fk . fmap runDTerm)
    . parseTagExpression

toFileSet :: TaggedConnection -> HashSet Tag -> IO (HashSet File)
toFileSet conn =
  foldM
    ( \acc rt ->
        maybe acc (`HS.insert` acc)
          <$> queryForFileByTagId rt conn
    )
    HS.empty
    . map tagId
    . HS.toList

-- Tagging Engine

{- |
 Where 'e` is 1-indexed in order of evaluation.
-}
class ExpressionIndex e where
  -- | Find a subset of 'e` at the given index if it exists.
  lookup :: Int -> e -> Maybe e

  -- | Replace a location in the second set with first, if that location exists.
  replace :: Int -> e -> e -> e

  -- | An infix variant of 'lookup`
  (!!) :: e -> Int -> Maybe e
  e !! n = lookup n e

{- |
 Lens for indexing an 'ExpressionIndex`
-}
exprAt :: ExpressionIndex e => Int -> Lens' e (Maybe e)
exprAt n =
  lens
    (lookup n)
    (\expr mReplace -> maybe expr (\re -> replace n re expr) mReplace)

{- |
 Helper function for defining instances of 'ExpressionIndex.lookup`
-}
stlookupWith ::
  (Monad m, Num a, Eq a) =>
  a ->
  b ->
  StateT (a, Maybe b) m b
stlookupWith n x = do
  st <- gets fst
  modify . first $ (1 +)
  when (st == n) . modify . second . const . Just $ x
  return x

{- |
 Helper function for defining instance of 'ExpressionIndex.replace`
-}
stReplaceWith :: (Monad m, Num a, Eq a) => a -> b -> b -> StateT a m b
stReplaceWith n t e = do
  st <- get
  modify (1 +)
  if st == n
    then return t
    else return e

{- |
 Flatten an 'ExpressionIndex` into its respective components along with their indices
 in reverse evaluation order.

 Such that:

 @e = (snd . head . flatten) e@
-}
flatten :: ExpressionIndex e => e -> [(Int, e)]
flatten = foldIxGen (flip (:)) []

{- |
 Generate an infinite indexed list in evaluation order from an expression.

 Calling @takeWhile (isJust . snd)@
  will return a finite list of the expression's contents.
-}
exprIxGen :: ExpressionIndex e => e -> [(Int, Maybe e)]
exprIxGen e = [(n, e !! n) | n <- [1 ..]]

{- |
 foldl' over a flattened 'ExpressionIndex` in reverse evaluation order.
-}
foldIxGen :: ExpressionIndex e => (a -> (Int, e) -> a) -> a -> e -> a
foldIxGen foldF accum = go foldF accum . exprIxGen
 where
  go _ acc [] = acc
  go _ acc ((_, Nothing) : _) = acc
  go f acc (x : xs) = go f (f acc . second fromJust $ x) xs

{- |
 Find the latest evaluating subset of the expression.
-}
index :: (ExpressionIndex a, Eq a) => a -> a -> Maybe (Int, a)
index needle = indexWith (== needle)

{- |
 Find the latest evaluating expression that satisfies the predicate.
-}
indexWith :: ExpressionIndex t => (t -> Bool) -> t -> Maybe (Int, t)
indexWith p = foldIxGen (\m x@(_, a) -> m <|> (x <$ guard (p a))) Nothing

queryQueryExpression :: TaggedConnection -> QueryExpression -> IO (HashSet File)
queryQueryExpression c =
  fmap evaluateRing
    . traverse (queryQueryLeaf c)
    . runQueryExpression

queryQueryLeaf :: TaggedConnection -> QueryLeaf -> IO (HashSet File)
queryQueryLeaf c ql = case ql of
  FileLeaf pat ->
    case pat of
      WildCard -> HS.fromList <$> allFiles c
      PatternText t -> HS.fromList <$> queryForFileByPattern t c
  TagLeaf te -> evaluateTagExpression c te >>= toFileSet c

-- A naive query interpreter, with no caching.
evaluateTagExpression ::
  TaggedConnection ->
  TagExpression (DTerm Pattern) ->
  IO (HashSet Tag)
evaluateTagExpression c =
  fmap
    ( evaluateTagExpressionR
        rightAssocJoinTags
        . distribute
    )
    . traverse (queryDTerm c)
 where
  rightAssocJoinTags supers subs = joinSubtags supers (HS.map tagSubtagOfId subs)

queryDTerm :: TaggedConnection -> DTerm Pattern -> IO (HashSet Tag)
queryDTerm c dt = case dt of
  DTerm (PatternText t) ->
    HS.fromList
      <$> queryForTagByDescriptorPattern t c
  DMetaTerm (PatternText t) ->
    HS.fromList
      <$> queryForTagByMetaDescriptorPattern t c
  _wildcard -> HS.fromList <$> allTags c

{- |
 A newtype used to provide a Rng instance when inserting tags defined by a
 TagExpression.
-}
newtype TagInserter = TagInserter
  {runTagInserter :: Maybe [RecordKey Tag] -> IO [RecordKey Tag]}

{- |
 Sequences two TagInserters and returns an empty list.
-}
instance Semigroup TagInserter where
  (<>) :: TagInserter -> TagInserter -> TagInserter
  (TagInserter x) <> (TagInserter y) =
    TagInserter $ \mrkt -> (x mrkt *> y mrkt) $> mempty

insertTagExpression ::
  TaggedConnection ->
  RecordKey File ->
  TagExpression Pattern ->
  IO ()
insertTagExpression c fk =
  void
    . flip runTagInserter Nothing
    . runDefaultRng
    . evaluateTagExpressionL (liftA2 leftAssocInsertTags)
    . distribute
    . fmap
      ( DefaultRng
          . TagInserter
          . insertTagPattern c fk
      )
 where
  {-
   Left-associative insertion of tags, where the TagInserter on the left
   is inserted first and its output is fed into the right TagInserter.
  -}
  leftAssocInsertTags (TagInserter x) (TagInserter y) = TagInserter (x >=> (y . Just))

{- |
 Insert descriptors matching the given pattern as Tags on the given file.
 If a list of Tag ID's is supplied then the new tags are inserted as subtags of those.

 Returns a list of Tag ID's corresponding to the descriptors matching the given pattern
 on the file that are subtags of the given list if one is provided.

 Does nothing if the given pattern is a wildcard.
-}
insertTagPattern ::
  TaggedConnection ->
  RecordKey File ->
  Pattern ->
  Maybe [RecordKey Tag] ->
  IO [RecordKey Tag]
insertTagPattern _ _ WildCard _ = pure mempty
insertTagPattern c fk (PatternText t) mrts = do
  withDescriptors <- queryForDescriptorByPattern t c
  let tagTriples =
        (fk,,) <$> map descriptorId withDescriptors <*> maybe [Nothing] (map Just) mrts

  void $ insertTags tagTriples c

  -- Tag insertion may fail because some tags of the same form already exist.
  -- This query gets all of those pre-existing tags,
  -- and returns them as if they were just made.
  case mrts of
    -- If nothing, then these are top-level tags
    Nothing ->
      map tagId <$> queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf fk t c
    -- If just, these are subtags of existing tags.
    Just _ ->
      let unions xs = if null xs then [] else L.foldl' L.union [] xs
          third f (x, y, z) = (x, y, f z)
       in map tagId . unions
            <$> mapM
              (`queryForTagBySubTagTriple` c)
              (third fromJust <$> tagTriples)
