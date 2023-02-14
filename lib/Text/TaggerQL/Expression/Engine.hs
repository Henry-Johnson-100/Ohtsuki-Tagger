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

  -- * New
  queryQueryExpression,
  insertTagExpression,
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, void, (<=<), (>=>))
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Bifunctor (Bifunctor (second), bimap)
import Data.Bitraversable (bitraverse)
import Data.Either (fromRight)
import qualified Data.Foldable as F
import Data.Functor (($>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Maybe (fromJust)
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
  queryForFileByPattern,
  queryForTagBySubTagTriple,
 )
import Database.Tagger.Type (
  File,
  Tag (tagId, tagSubtagOfId),
  TaggedConnection,
  descriptorId,
 )
import Lens.Micro (Lens', lens)
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

queryQueryExpression ::
  TaggedConnection ->
  FreeQueryExpression ->
  IO (HashSet File)
queryQueryExpression c =
  fmap evaluateRing
    . resolveTagFileDisjunction
    <=< queryTerms
      . unrecurse
 where
  resolveTagFileDisjunction = traverse (either pure (toFileSet c))

  queryTerms =
    traverse (bitraverse (queryFilePattern c) (evaluateTagExpression c))

  -- Resolves the first disjunction by left distribution of the left
  -- product type so that the query language can be
  -- represented as a non-recursive type.
  unrecurse = either unify pure <=< runFreeQueryExpression
   where
    unify (FreeQueryExpression fqe', tqe) =
      fqe'
        >>= either
          unify
          ( either
              ((*. (Ring . Right $ tqe)) . Ring . Left)
              (Ring . Right . (∙ tqe))
          )

queryFilePattern :: TaggedConnection -> Pattern -> IO (HashSet File)
queryFilePattern c pat =
  case pat of
    WildCard -> HS.fromList <$> allFiles c
    PatternText t -> HS.fromList <$> queryForFileByPattern t c

-- A naive query interpreter, with no caching.
evaluateTagExpression ::
  TaggedConnection ->
  TagQueryExpression ->
  IO (HashSet Tag)
evaluateTagExpression c =
  fmap
    ( evaluateRing
        . fmap (F.foldr1 rightAssocJoinTags)
        . distributeK
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
  FreeCompoundExpression RingExpression FreeMagma Pattern ->
  IO ()
insertTagExpression c fk =
  void
    . flip runTagInserter Nothing
    . F.foldl1 (<>)
    . fmap (F.foldl1 leftAssocInsertTags)
    . distributeK
    . fmap
      ( TagInserter
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
