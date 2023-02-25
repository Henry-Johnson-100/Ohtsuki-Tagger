{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use <=<" #-}
{-# LANGUAGE ViewPatterns #-}
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
  fileQuery,
  tagFile,
  formatQueryExpression,

  -- * New
  runFileQuery,
  runTagFile,
) where

import Control.Monad (void, (<=<), (>=>))
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
import Text.TaggerQL.Expression.AST (
  DTerm (DMetaTerm, DTerm),
  FreeDisjunctMonad,
  LabeledFreeTree (..),
  MagmaExpression,
  Pattern (PatternText, WildCard),
  QueryExpression,
  RingExpression,
  RingOperation (..),
  TagQueryExpression,
  TraversableQueryExpression (runTraversableQueryExpression),
  distributeK,
  evaluateFreeCompoundExpression,
  evaluateRingExpression,
  patternText,
  runDTerm,
  simplifyQueryExpression,
 )
import Text.TaggerQL.Expression.Parser (parseQueryExpression, parseTagExpression)

{- |
 Run a TaggerQL query on the given database.
-}
fileQuery :: TaggedConnection -> Text -> IO (Either [Text] (HashSet File))
fileQuery c =
  either
    (pure . Left . map (T.pack . messageString) . errorMessages)
    (fmap pure . runFileQuery c)
    . parseQueryExpression

{- |
 Tag a file with the given 'TagExpression`
-}
tagFile :: RecordKey File -> TaggedConnection -> Text -> IO (Maybe Text)
tagFile fk c =
  either
    (pure . Just . T.pack . show)
    (fmap (const Nothing) . runTagFile c fk . fmap runDTerm)
    . parseTagExpression

runFileQuery ::
  TaggedConnection ->
  QueryExpression ->
  IO (HashSet File)
runFileQuery c =
  fmap evaluateRingExpression
    . traverse (either pure toFileSet)
    <=< fmap
      ( fmap
          ( fmap
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
    let subtagIds = HS.map tagSubtagOfId subTagSet
     in HS.filter (flip HS.member subtagIds . Just . tagId) superTagSet

-- Tagging Engine

newtype TagInserter = TagInserter
  {runTagInserter :: Maybe [RecordKey Tag] -> IO [RecordKey Tag]}

runTagFile ::
  TaggedConnection ->
  RecordKey File ->
  FreeDisjunctMonad RingExpression MagmaExpression Pattern ->
  IO ()
runTagFile c fk =
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

-- formatQueryExpression :: QueryExpression -> Text
-- formatQueryExpression (runTraversableQueryExpression -> tqe) =
--   catLFT (\ro rhs -> formatRo ro <> rhs) inParens $
--     do
--       term <- tqe
--       case term of
--         Left (tqe', b) ->
--           pure $
--             inParens (formatQueryExpression tqe')
--               <> inBrackets (formatTagQueryExpression b)
--         Right e -> pure $ either patternText formatTagQueryExpression e
--  where
--   formatTagQueryExpression =
--     evaluateFreeCompoundExpression
--       (inParens . catLFT (\ro rhs -> formatRo ro <> rhs) inParens)
--       (catLFT (\_ rhs -> inBrackets rhs) id)
--       . fmap
--         ( \dt -> case dt of
--             DTerm p -> "d." <> patternText p
--             DMetaTerm p -> patternText p
--         )
--   catLFT formatL formatRHS lft =
--     case lft of
--       Node x -> x
--       Edge lhs l rhs ->
--         catLFT formatL formatRHS lhs
--           <> formatL
--             l
--             ( case rhs of
--                 Node y -> y
--                 Edge{} -> formatRHS (catLFT formatL formatRHS rhs)
--             )

type FCont = (Text -> Text) -> Text

{- |
 Modifies text based on a condition
-}
type FCondCont = (Bool -> Text -> Text) -> Text

liftText :: Text -> FCont
liftText t ret = ret t

liftFCont :: FCont -> FCondCont
liftFCont f ret = f (ret False)

formatQueryExpression :: QueryExpression -> Text
formatQueryExpression
  (runTraversableQueryExpression -> tqe) =
    case tqe of
      Node e -> formatNode e
      Edge
        ulhs
        (formatRo -> ro)
        urhs ->
          let lhsF =
                formatRingExpression
                  (fmap (liftFCont . liftText . formatNode) ulhs)
                  (const id)
              rhsF =
                formatRingExpression
                  (fmap (liftFCont . liftText . formatNode) urhs)
                  (\b t -> if b then inParens t id else t)
           in lhsF <> ro <> rhsF
   where
    formatNode e = case e of
      Left (qe', te') ->
        let qe'F =
              inParens
                ( formatQueryExpression
                    qe'
                )
                id
                <> inBrackets (formatTagExpression te') id
         in qe'F
      Right e' ->
        either
          (\p -> "p." <> formatPattern p)
          (flip inBrackets id . formatTagExpression)
          e'

formatTagExpression :: TagQueryExpression -> Text
formatTagExpression tqe =
  evaluateFreeCompoundExpression
    formatRingExpression
    formatMagmaExpression
    (fmap (liftFCont . liftText . formatDTerm) tqe)
    (const id)
 where
  formatMagmaExpression :: MagmaExpression FCondCont -> FCondCont
  formatMagmaExpression me cret = case me of
    Node f -> f cret
    Edge
      (formatMagmaExpression -> lhs)
      ()
      (formatMagmaExpression -> rhs) ->
        let lhsF = lhs (\b t -> if b then inParens t id else t)
            rhsF = inBrackets (rhs (const id)) id
         in cret False $ lhsF <> rhsF

  formatDTerm dt =
    case dt of
      DTerm p -> "d." <> formatPattern p
      DMetaTerm p -> formatPattern p

formatRingExpression :: RingExpression FCondCont -> FCondCont
formatRingExpression re cret = case re of
  Node f -> f cret
  Edge
    (formatRingExpression -> lhs)
    (formatRo -> ro)
    (formatRingExpression -> rhs) ->
      let lhsF = lhs (const id)
          rhsF = rhs (\b t -> if b then inParens t id else t)
       in cret True $ lhsF <> ro <> rhsF

formatPattern :: Pattern -> Text
formatPattern = patternText

formatRo :: RingOperation -> Text
formatRo ro =
  case ro of
    Addition -> " | "
    Multiplication -> " & "
    Subtraction -> " ! "

inParens :: Text -> (Text -> Text) -> Text
inParens x ret = "(" <> ret x <> ")"

inBrackets :: Text -> (Text -> Text) -> Text
inBrackets x ret = "{" <> ret x <> "}"
