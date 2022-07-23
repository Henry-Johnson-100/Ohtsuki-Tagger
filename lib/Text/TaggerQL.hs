{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use concatMap" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL
Description : The front-end and interface for running TaggerQL queries on a Tagger database.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL (
  -- * Query with TaggerQL
  TaggerQLQuery (..),
  taggerQL,
  queryRequest,

  -- * Tag with TaggerQL
  TaggerQLTagStmnt (..),
  taggerQLTag,

  -- * Other
  CombinableSentenceResult,
  combinableSentenceResultSetOp,
  combinableSentenceResultSet,
) where

import Control.Monad (void, (<=<))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import Data.Tagger (
  QueryCriteria (
    DescriptorCriteria,
    FilePatternCriteria,
    MetaDescriptorCriteria,
    UntaggedCriteria
  ),
  SetOp (..),
 )
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Query (
  flatQueryForFileByTagDescriptorPattern,
  flatQueryForFileOnMetaRelationPattern,
  insertTags,
  queryForDescriptorByPattern,
  queryForFileByPattern,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagBySubTagTriple,
  queryForUntaggedFiles,
 )
import Database.Tagger.Type (
  Descriptor (descriptorId),
  File,
  RecordKey,
  Tag (tagId),
 )
import System.IO (hPrint, stderr)
import Text.TaggerQL.AST (
  ComplexTerm (..),
  Request (Request),
  Sentence (Sentence),
  SentenceSet (..),
  SentenceTree (..),
  SimpleTerm (..),
  Term (Term),
  TermTree (..),
  complexTermNode,
 )
import Text.TaggerQL.Engine.QueryEngine
import Text.TaggerQL.Engine.QueryEngine.Type
import Text.TaggerQL.Parser.Internal (
  parse,
  requestParser,
  sentenceParser,
 )

data CombinableSentenceResult
  = CombinableSentenceResult SetOp (HashSet File)
  deriving (Show, Eq)

combinableSentenceResultSetOp :: CombinableSentenceResult -> SetOp
combinableSentenceResultSetOp (CombinableSentenceResult so _) = so

combinableSentenceResultSet :: CombinableSentenceResult -> HashSet File
combinableSentenceResultSet (CombinableSentenceResult _ s) = s

{- |
 newtype wrapper for 'Text`

 is an instance of IsString
-}
newtype TaggerQLQuery = TaggerQLQuery Text deriving (Show, Eq)

instance IsString TaggerQLQuery where
  fromString = TaggerQLQuery . T.pack

{- |
 newtype wrapper for 'Text`

 is an instance of IsString
-}
newtype TaggerQLTagStmnt = TaggerQLTagStmnt Text deriving (Show, Eq)

instance IsString TaggerQLTagStmnt where
  fromString = TaggerQLTagStmnt . T.pack

newtype TermResult = TermResult {termResult :: HashSet File} deriving (Show, Eq)

newtype TermTag = TermTag (Term Text) deriving (Show, Eq)

newtype TermSubTag = TermSubTag (Term Text) deriving (Show, Eq)

{- |
 Run a 'TaggerQLQuery` on a connection.
-}
taggerQL :: TaggerQLQuery -> TaggedConnection -> IO (HashSet File)
taggerQL (TaggerQLQuery q) tc = do
  let parseResult = parse requestParser "TaggerQL" q
  either
    (const (return HS.empty) <=< hPrint stderr)
    (fmap combinableSentenceResultSet . queryRequest tc)
    parseResult

{- |
 Run a query given the 'Request` AST of the result of parsing a 'TaggerQLQuery`

 Can be used to programmatically generate queries using TaggerQL alongside the
 Text.TaggerQL.AST module to create the structure.
-}
queryRequest ::
  TaggedConnection ->
  Request Text ->
  IO CombinableSentenceResult
queryRequest tc (Request strs) = combineSentences <$> mapM (querySentenceTree tc) strs

querySentenceTree ::
  TaggedConnection ->
  SentenceTree Text ->
  IO CombinableSentenceResult
querySentenceTree tc tr =
  case tr of
    SentenceNode ss -> querySentenceSet tc ss
    SentenceBranch so sss -> do
      (CombinableSentenceResult _ sentenceResults) <-
        combineSentences <$> mapM (querySentenceTree tc) sss
      return $ CombinableSentenceResult so sentenceResults

querySentenceSet ::
  TaggedConnection ->
  SentenceSet Text ->
  IO CombinableSentenceResult
querySentenceSet tc (CombinableSentence so s) =
  CombinableSentenceResult so . termResult <$> querySentence tc s

{- |
 All 'TermTree`s in a 'Sentence` are intersected with each other.
-}
querySentence ::
  TaggedConnection ->
  Sentence Text ->
  IO TermResult
querySentence tc (Sentence tts) =
  intersectTermResults
    <$> mapM (queryTermTree tc) tts

{- |
 Return 'Nothing` if the given 'TermTree` is Complex and Bottom.
-}
queryTermTree ::
  TaggedConnection ->
  TermTree Text ->
  IO TermResult
queryTermTree tc tt =
  case tt of
    Simple st -> querySimpleTerm tc st
    Complex ct ->
      TermResult
        <$> runReaderT (queryComplexTermTopLevel ct) (QueryEnv mempty tc)

querySimpleTerm ::
  TaggedConnection ->
  SimpleTerm Text ->
  IO TermResult
querySimpleTerm tc (SimpleTerm (Term qc p)) =
  case qc of
    DescriptorCriteria ->
      TermResult . HS.fromList <$> flatQueryForFileByTagDescriptorPattern p tc
    MetaDescriptorCriteria ->
      TermResult . HS.fromList <$> flatQueryForFileOnMetaRelationPattern p tc
    FilePatternCriteria ->
      TermResult . HS.fromList <$> queryForFileByPattern p tc
    UntaggedCriteria ->
      TermResult . HS.fromList <$> queryForUntaggedFiles tc

{- |
 Should perform an associative strict intersection of the given 'TermResult`s.
-}
intersectTermResults :: [TermResult] -> TermResult
intersectTermResults [] = emptyTermResult
intersectTermResults trs = L.foldl1' intersectTermResult trs

intersectTermResult :: TermResult -> TermResult -> TermResult
intersectTermResult (TermResult x) (TermResult y) = TermResult $ HS.intersection x y

combineSentences :: [CombinableSentenceResult] -> CombinableSentenceResult
combineSentences [] = emptyCombinableSentenceResult
combineSentences cs = L.foldl1' combineSentence cs

{- |
 Left-associative combination of the results of a combinable sentence query.
-}
combineSentence ::
  CombinableSentenceResult ->
  CombinableSentenceResult ->
  CombinableSentenceResult
combineSentence
  (CombinableSentenceResult sox sx)
  (CombinableSentenceResult soy sy) = CombinableSentenceResult sox $ combFun soy sx sy

{- |
 Return an empty result with a default 'SetOp` of Union.
-}
emptyCombinableSentenceResult :: CombinableSentenceResult
emptyCombinableSentenceResult = CombinableSentenceResult Union HS.empty

emptyTermResult :: TermResult
emptyTermResult = TermResult HS.empty

combFun ::
  Hashable a =>
  SetOp ->
  HashSet a ->
  HashSet a ->
  HashSet a
combFun so =
  case so of
    Union -> HS.union
    Intersect -> HS.intersection
    Difference -> HS.difference

type TaggingEnvironment = (TaggedConnection, [RecordKey Tag])

type TaggingReader a = ReaderT TaggingEnvironment IO a

setTagList :: [RecordKey Tag] -> TaggingEnvironment -> TaggingEnvironment
setTagList t (c, _) = (c, t)

{- |
 Run a subset of the TaggerQL as 'Descriptor` patterns to insert as tags on the given
 'File`.

 Because it is a subset of TaggerQL, 'QueryCriteria` Literals can be parsed, but they
 will be ignored during insertion. If it is desired to 'Tag` a 'File` with 'Descriptor`s
 that look like 'QueryCriteria` Literals, then use escape characters.
-}
taggerQLTag :: RecordKey File -> TaggerQLTagStmnt -> TaggedConnection -> IO ()
taggerQLTag fk (TaggerQLTagStmnt q) tc = do
  let tagStmnt = parse sentenceParser "TaggerQLStmnt" q
  either
    (hPrint stderr)
    (insertTagSentence tc . zipDescriptorPatternsWithFileKey fk)
    tagStmnt
 where
  zipDescriptorPatternsWithFileKey ::
    RecordKey File ->
    Sentence Text ->
    Sentence (RecordKey File, Text)
  zipDescriptorPatternsWithFileKey fk' = fmap (fk',)

insertTagSentence :: TaggedConnection -> Sentence (RecordKey File, Text) -> IO ()
insertTagSentence tc (Sentence tts) = mapM_ (insertTagTree tc) tts

insertTagTree :: TaggedConnection -> TermTree (RecordKey File, Text) -> IO ()
insertTagTree tc tt =
  case tt of
    Simple t -> void (runReaderT (insertSimpleTerm t) (tc, []))
    Complex t ->
      void
        (runReaderT (insertComplexTerm True t) (tc, []))

insertComplexTerm ::
  Bool -> ComplexTerm (RecordKey File, Text) -> TaggingReader [RecordKey Tag]
insertComplexTerm _ (Bottom _) = return []
insertComplexTerm True ct = do
  newEnvList <- insertTopLevelComplexTerm ct
  local (setTagList newEnvList) $ insertComplexTerm False ct
 where
  insertTopLevelComplexTerm ::
    ComplexTerm (RecordKey File, Text) ->
    TaggingReader [RecordKey Tag]
  insertTopLevelComplexTerm (complexTermNode -> t@(Term _ (fk, p))) = do
    void $ insertTerm t
    c <- asks fst
    map tagId
      <$> lift
        ( queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf
            fk
            p
            c
        )
insertComplexTerm _ (currentTerm :<- (ct :| sts)) = do
  void $ case ct of
    Bottom t -> insertSubTag t
    (complexTermNode -> ctt) -> do
      lowerLayerEnv <- insertSubTag ctt
      local (setTagList lowerLayerEnv) $ insertComplexTerm False ct
  mapM_ (\ct' -> insertComplexTerm False (currentTerm :<- (ct' :| []))) sts
  return []

insertSubTag ::
  Term (RecordKey File, Text) ->
  ReaderT TaggingEnvironment IO [RecordKey Tag]
insertSubTag (Term _ (fk, p)) = do
  te <- ask
  subTagDescriptorIds <-
    map descriptorId
      <$> lift (queryForDescriptorByPattern p (fst te))
  let subTagTriples = (fk,,) <$> subTagDescriptorIds <*> (Just <$> snd te)
  lift . void $ insertTags subTagTriples (fst te)
  map tagId . unions
    <$> lift
      ( mapM
          (`queryForTagBySubTagTriple` fst te)
          (third fromJust <$> subTagTriples)
      )
 where
  third f (x, y, z) = (x, y, f z)

insertSimpleTerm ::
  SimpleTerm (RecordKey File, Text) ->
  TaggingReader [RecordKey Tag]
insertSimpleTerm (SimpleTerm t) = insertTerm t

insertTerm :: Term (RecordKey File, Text) -> TaggingReader [RecordKey Tag]
insertTerm (Term _ (fk, p)) = do
  c <- asks fst
  ds <- map descriptorId <$> lift (queryForDescriptorByPattern p c)
  let tagTriples = (fk,,Nothing) <$> ds
  lift $ insertTags tagTriples c

unions :: (Foldable t, Eq a) => t [a] -> [a]
unions xs = if null xs then [] else L.foldl' L.union [] xs
