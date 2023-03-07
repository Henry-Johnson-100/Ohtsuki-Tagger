{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use <=<" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune #-}

{-# HLINT ignore "Use const" #-}

{- |
Module      : Text.TaggerQL.Expression.Engine
Description : The interpreter for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Contains functions that interprets the TaggerQL query language to either run queries
  or tag a file with a certain expression.
-}
module Text.TaggerQL.Expression.Engine (
  yuiQLFileQuery,
  yuiQLTagFile,
  yuiQLDeleteTags,

  -- * New
  yuiQLFileQueryExpression,
  yuiQLTagFileExpression,
  yuiQLDeleteTagExpression,
) where

import Control.Monad (void, (<=<), (>=>))
import Data.Bifunctor (first)
import Data.Bitraversable (bitraverse)
import qualified Data.Foldable as F
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
  deleteTags,
  insertTags,
  isSubTagOf,
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
import Text.TaggerQL.Expression.AST (
  DTerm (DMetaTerm, DTerm),
  Pattern (PatternText, WildCard),
  QueryExpression,
  TagDeleteExpression,
  TagFileExpression,
  distributeK,
  evaluateRingExpression,
  runDTerm,
  simplifyQueryExpression,
 )
import Text.TaggerQL.Expression.Parser (parseQueryExpression, parseTagExpression)

{- |
 Run a YuiQL query on the given database.
-}
yuiQLFileQuery :: TaggedConnection -> Text -> IO (Either [Text] (HashSet File))
yuiQLFileQuery c =
  either
    (pure . Left . map (T.pack . messageString) . errorMessages)
    (fmap pure . yuiQLFileQueryExpression c)
    . parseQueryExpression

{- |
 Tag a file with the given 'TagExpression`
-}
yuiQLTagFile :: RecordKey File -> TaggedConnection -> Text -> IO (Maybe Text)
yuiQLTagFile fk c =
  either
    (pure . Just . T.pack . show)
    (fmap (const Nothing) . yuiQLTagFileExpression c fk . fmap runDTerm)
    . parseTagExpression

yuiQLFileQueryExpression ::
  TaggedConnection ->
  QueryExpression ->
  IO (HashSet File)
yuiQLFileQueryExpression c =
  fmap evaluateRingExpression
    . traverse (either pure toFileSet)
    <=< fmap
      ( fmap
          (fmap joinTagQueryResultSets)
          . simplifyQueryExpression
      )
      . bitraverse queryFilePattern (traverse (queryDTerm c))
 where
  queryFilePattern pat =
    case pat of
      WildCard -> HS.fromList <$> allFiles c
      PatternText t -> HS.fromList <$> queryForFileByPattern t c

  toFileSet =
    HS.foldl'
      ( \acc fk ->
          (\x -> maybe x (`HS.insert` x))
            <$> acc
              <*> queryForSingleFileByFileId fk c
      )
      (pure HS.empty)
      . HS.map tagFileId

  joinTagQueryResultSets = evaluateRingExpression . fmap (F.foldr1 tagMagma) . distributeK
   where
    tagMagma superTagSet subTagSet =
      let subtagIds = HS.map tagSubtagOfId subTagSet
       in HS.filter (flip HS.member subtagIds . Just . tagId) superTagSet

queryDTerm :: TaggedConnection -> DTerm Pattern -> IO (HashSet Tag)
queryDTerm c dt = case dt of
  DTerm (PatternText t) ->
    HS.fromList
      <$> queryForTagByDescriptorPattern t c
  DMetaTerm (PatternText t) ->
    HS.fromList
      <$> queryForTagByMetaDescriptorPattern t c
  _wildcard -> HS.fromList <$> allTags c

-- Tagging Engine

newtype TagInserter = TagInserter
  {runTagInserter :: Maybe [RecordKey Tag] -> IO [RecordKey Tag]}

yuiQLTagFileExpression ::
  TaggedConnection ->
  RecordKey File ->
  TagFileExpression ->
  IO ()
yuiQLTagFileExpression c fk =
  void
    . flip runTagInserter Nothing
    . F.foldl1 sequenceTagInserters
    . fmap (F.foldl1 leftAssocInsertTags)
    . distributeK
    . fmap
      ( TagInserter
          . insertTagPattern
      )
 where
  {-
  Run an expression of TagInserters left-to-right, ignoring their results.
  -}
  sequenceTagInserters (TagInserter x) (TagInserter y) =
    TagInserter $ \mrkt -> x mrkt *> y mrkt

  {-
   Left-associative insertion of tags, where the TagInserter on the left
   is inserted first and its output is fed into the right TagInserter.
  -}
  leftAssocInsertTags (TagInserter x) (TagInserter y) =
    TagInserter (x >=> (y . Just))

  {-
  Insert descriptors matching the given pattern as Tags on the given file.
  If a list of Tag ID's is supplied then the new tags are inserted as subtags of those.

  Returns a list of Tag ID's corresponding to the descriptors matching the given pattern
  on the file that are subtags of the given list if one is provided.

  Does nothing if the given pattern is a wildcard.
  -}
  insertTagPattern WildCard _ = pure mempty
  insertTagPattern (PatternText t) mrts = do
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

yuiQLDeleteTags ::
  TaggedConnection ->
  [RecordKey File] ->
  Text ->
  IO (Either Text ())
yuiQLDeleteTags c fks t =
  let parsedTagExpression = parseTagExpression t
   in traverse (yuiQLDeleteTagExpression c fks . fmap runDTerm) . first (T.pack . show) $
        parsedTagExpression

yuiQLDeleteTagExpression ::
  TaggedConnection ->
  [RecordKey File] ->
  TagDeleteExpression ->
  IO ()
yuiQLDeleteTagExpression _ [] _ = pure ()
yuiQLDeleteTagExpression c fks tqe = do
  tagsToDelete <- yuiQLQueryTagDeleteExpression c fks tqe
  deleteTags (map tagId . HS.toList $ tagsToDelete) c

{- |
 Performs a query for tags that will be deleted by the given expression.
-}
yuiQLQueryTagDeleteExpression ::
  TaggedConnection ->
  [RecordKey File] ->
  TagDeleteExpression ->
  IO (HashSet Tag)
yuiQLQueryTagDeleteExpression _ [] _ = pure HS.empty
yuiQLQueryTagDeleteExpression c fks tqe = do
  traversedTQE <-
    fmap (HS.filter ((`elem` fks) . tagFileId))
      <$> traverse
        ( \p ->
            HS.fromList
              <$> case p of
                WildCard -> allTags c
                PatternText txt -> queryForTagByDescriptorPattern txt c
        )
        tqe

  let distributedTQE = distributeK traversedTQE
      foldedMagmas =
        fmap
          ( F.foldl1
              ( \l r ->
                  HS.filter
                    ( \rt ->
                        HS.foldl' (\b lt -> b || rt `isSubTagOf` lt) False l
                    )
                    r
              )
          )
          distributedTQE
      result = F.foldl1 HS.union foldedMagmas

  pure result