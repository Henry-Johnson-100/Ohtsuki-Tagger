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

import Control.Monad (void, (<=<), (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Bifunctor (second)
import Data.Bitraversable (bitraverse)
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
  queryForSingleFileByFileId,
  queryForTagByDescriptorPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagByMetaDescriptorPattern,
  tagFileId,
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

queryQueryExpression ::
  TaggedConnection ->
  QueryExpression ->
  IO (HashSet File)
queryQueryExpression c =
  fmap evaluateRingExpression
    . traverse (either pure toFileSet)
    <=< fmap
      ( fmap
          ( second
              ( evaluateRingExpression
                  . fmap (F.foldr1 tagMagma)
                  . distributeK
              )
          )
          . simplifyQueryExpression
      )
      . bitraverse queryFilePattern (traverse queryDTerm)
 where
  queryFilePattern pat =
    case pat of
      WildCard -> HS.fromList <$> allFiles c
      PatternText t -> HS.fromList <$> queryForFileByPattern t c

  queryDTerm dt = case dt of
    DTerm (PatternText t) ->
      HS.fromList
        <$> queryForTagByDescriptorPattern t c
    DMetaTerm (PatternText t) ->
      HS.fromList
        <$> queryForTagByMetaDescriptorPattern t c
    _wildcard -> HS.fromList <$> allTags c

  toFileSet =
    HS.foldl'
      ( \acc fk ->
          (\x -> maybe x (`HS.insert` x))
            <$> acc
              <*> queryForSingleFileByFileId fk c
      )
      (pure HS.empty)
      . HS.map tagFileId

  tagMagma superTagSet subTagSet =
    joinSubtags superTagSet (HS.map tagSubtagOfId subTagSet)

-- Tagging Engine

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
  FreeDisjunctMonad RingExpression MagmaExpression Pattern ->
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
