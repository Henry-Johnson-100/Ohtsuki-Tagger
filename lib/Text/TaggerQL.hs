{-# LANGUAGE TupleSections #-}
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
  TaggerQLQuery,
  taggerQL,
  queryRequest,

  -- * Tag with TaggerQL
  TaggerQLTagStmnt,
  taggerQLTag,

  -- * Other
  CombinableSentenceResult,
  combinableSentenceResultSetOp,
  combinableSentenceResultSet,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.String
import Data.Tagger
import qualified Data.Text as T
import Database.Tagger
import System.IO
import Tagger.Util
import Text.TaggerQL.AST
import Text.TaggerQL.Parser.Internal

data CombinableSentenceResult
  = CombinableSentenceResult SetOp (HashSet.HashSet File)
  deriving (Show, Eq)

combinableSentenceResultSetOp :: CombinableSentenceResult -> SetOp
combinableSentenceResultSetOp (CombinableSentenceResult so _) = so

combinableSentenceResultSet :: CombinableSentenceResult -> HashSet.HashSet File
combinableSentenceResultSet (CombinableSentenceResult _ s) = s

{- |
 newtype wrapper for 'Text`

 is an instance of IsString
-}
newtype TaggerQLQuery = TaggerQLQuery T.Text deriving (Show, Eq)

instance IsString TaggerQLQuery where
  fromString = TaggerQLQuery . T.pack

{- |
 newtype wrapper for 'Text`

 is an instance of IsString
-}
newtype TaggerQLTagStmnt = TaggerQLTagStmnt T.Text deriving (Show, Eq)

instance IsString TaggerQLTagStmnt where
  fromString = TaggerQLTagStmnt . T.pack

newtype TermResult = TermResult {termResult :: HashSet.HashSet File} deriving (Show, Eq)

newtype TermTag = TermTag (Term T.Text) deriving (Show, Eq)

newtype TermSubTag = TermSubTag (Term T.Text) deriving (Show, Eq)

{- |
 Run a 'TaggerQLQuery` on a connection.
-}
taggerQL :: TaggerQLQuery -> TaggedConnection -> IO (HashSet.HashSet File)
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
  Request T.Text ->
  IO CombinableSentenceResult
queryRequest tc (Request strs) = combineSentences <$> mapM (querySentenceTree tc) strs

querySentenceTree ::
  TaggedConnection ->
  SentenceTree T.Text ->
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
  SentenceSet T.Text ->
  IO CombinableSentenceResult
querySentenceSet tc (CombinableSentence so s) =
  CombinableSentenceResult so . termResult <$> querySentence tc s

{- |
 All 'TermTree`s in a 'Sentence` are intersected with each other.
-}
querySentence ::
  TaggedConnection ->
  Sentence T.Text ->
  IO TermResult
querySentence tc (Sentence tts) =
  intersectTermResults
    <$> catMaybeTM (queryTermTree tc) tts

{- |
 Return 'Nothing` if the given 'TermTree` is Complex and Bottom.
-}
queryTermTree ::
  TaggedConnection ->
  TermTree T.Text ->
  MaybeT IO TermResult
queryTermTree tc tt =
  case tt of
    Simple st -> lift $ querySimpleTerm tc st
    Complex ct -> queryComplexTerm tc ct

queryComplexTerm ::
  TaggedConnection ->
  ComplexTerm T.Text ->
  MaybeT IO TermResult
queryComplexTerm tc ct =
  case ct of
    -- should never be called
    Bottom _ -> hoistMaybe Nothing
    t :<- ((Bottom bst) NE.:| sts) ->
      case sts of
        -- Base terminal case for all nested complex term queries
        [] -> queryComplexTermRelation tc (TermTag t) (TermSubTag bst)
        sts' -> do
          thisResult <- queryComplexTermRelation tc (TermTag t) (TermSubTag bst)
          nextResults <-
            lift $
              intersectTermResults
                <$> catMaybeTM (\ct' -> queryComplexTerm tc (t :<- (ct' NE.:| []))) sts'
          let combinedResults = intersectTermResult thisResult nextResults
          return combinedResults
    t :<- (st NE.:| sts) ->
      case sts of
        [] -> do
          thisResult <-
            queryComplexTermRelation tc (TermTag t) (TermSubTag . complexTermNode $ st)
          nestedResult <- queryComplexTerm tc st
          let combinedResults = intersectTermResult thisResult nestedResult
          return combinedResults
        sts' -> do
          thisResult <-
            queryComplexTermRelation tc (TermTag t) (TermSubTag . complexTermNode $ st)
          nestedResult <- queryComplexTerm tc st
          nextResult <-
            lift $
              intersectTermResults
                <$> catMaybeTM (\ct' -> queryComplexTerm tc (t :<- (ct' NE.:| []))) sts'
          let combinedResults =
                intersectTermResults [thisResult, nestedResult, nextResult]
          return combinedResults

-- Could be handled easier programmatically, but with hand-written queries for
-- each case, it probably has better performance.
queryComplexTermRelation ::
  TaggedConnection ->
  TermTag ->
  TermSubTag ->
  MaybeT IO TermResult
queryComplexTermRelation
  tc
  (TermTag (Term tqc basisPattern))
  (TermSubTag (Term sqc subQueryPattern)) =
    case (tqc, sqc) of
      (DescriptorCriteria, DescriptorCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByDescriptorSubTagDescriptor basisPattern subQueryPattern tc
      (DescriptorCriteria, MetaDescriptorCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByDescriptorSubTagMetaDescriptor
              basisPattern
              subQueryPattern
              tc
      -- flipped case of (FilePatternCriteria, DescriptorCriteria)
      (DescriptorCriteria, FilePatternCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByFilePatternAndDescriptor subQueryPattern basisPattern tc
      (MetaDescriptorCriteria, DescriptorCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByMetaDescriptorSubTagDescriptor
              basisPattern
              subQueryPattern
              tc
      (MetaDescriptorCriteria, MetaDescriptorCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByMetaDescriptorSubTagMetaDescriptor
              basisPattern
              subQueryPattern
              tc
      -- flipped case of (FilePatternCriteria, MetaDescriptorCriteria)
      (MetaDescriptorCriteria, FilePatternCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByFilePatternAndMetaDescriptor subQueryPattern basisPattern tc
      (FilePatternCriteria, DescriptorCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByFilePatternAndDescriptor basisPattern subQueryPattern tc
      (FilePatternCriteria, MetaDescriptorCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByFilePatternAndMetaDescriptor basisPattern subQueryPattern tc
      (FilePatternCriteria, FilePatternCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByFilePatternAndFilePattern basisPattern subQueryPattern tc
      (FilePatternCriteria, UntaggedCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByFilePatternAndUntagged basisPattern tc
      -- flipped case of (FilePatternCriteria, UntaggedCriteria)
      (UntaggedCriteria, FilePatternCriteria) ->
        lift $
          TermResult . HS.fromList
            <$> queryForFileByFilePatternAndUntagged subQueryPattern tc
      (UntaggedCriteria, _) -> hoistMaybe Nothing
      (_, UntaggedCriteria) -> hoistMaybe Nothing

querySimpleTerm ::
  TaggedConnection ->
  SimpleTerm T.Text ->
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
  HashSet.HashSet a ->
  HashSet.HashSet a ->
  HashSet.HashSet a
combFun so =
  case so of
    Union -> HashSet.union
    Intersect -> HashSet.intersection
    Difference -> HashSet.difference

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
    Sentence T.Text ->
    Sentence (RecordKey File, T.Text)
  zipDescriptorPatternsWithFileKey fk' = fmap (fk',)

insertTagSentence :: TaggedConnection -> Sentence (RecordKey File, T.Text) -> IO ()
insertTagSentence tc (Sentence tts) = mapM_ (insertTagTree tc) tts

insertTagTree :: TaggedConnection -> TermTree (RecordKey File, T.Text) -> IO ()
insertTagTree tc tt =
  case tt of
    Simple t -> insertSimpleTerm tc t
    Complex t -> insertComplexTerm tc Nothing t

insertComplexTerm ::
  TaggedConnection ->
  Maybe (RecordKey Descriptor) ->
  ComplexTerm (RecordKey File, T.Text) ->
  IO ()
insertComplexTerm tc maybePrevBasisDK ct =
  case ct of
    (Bottom t) -> insertTerm tc maybePrevBasisDK t
    (Term _ (fk, p)) :<- sts -> do
      -- insert the top-level tags (as subtags if necessary)
      thisDks <- map descriptorId <$> queryForDescriptorByPattern p tc
      superDks <-
        maybe
          (pure [Nothing])
          (\bdk -> map (Just . tagId) <$> queryForTagByFileAndDescriptorKey fk bdk tc)
          maybePrevBasisDK
      let thisTagTuples = (fk,,) <$> thisDks <*> superDks
      void $ insertTags thisTagTuples tc
      -- map this function over the subterms with the current node's descriptors as the
      -- basis
      let subterms = NE.toList sts
      sequence_ $ insertComplexTerm tc . Just <$> thisDks <*> subterms

insertSimpleTerm :: TaggedConnection -> SimpleTerm (RecordKey File, T.Text) -> IO ()
insertSimpleTerm tc (SimpleTerm t) =
  insertTerm tc Nothing t

insertTerm ::
  TaggedConnection ->
  Maybe (RecordKey Descriptor) ->
  Term (RecordKey File, T.Text) ->
  IO ()
insertTerm tc maybeBasisDK (Term _ (fk, p)) = do
  thisDks <- map descriptorId <$> queryForDescriptorByPattern p tc
  subTagIds <-
    maybe
      (pure [Nothing])
      (\bdk -> map (Just . tagId) <$> queryForTagByFileAndDescriptorKey fk bdk tc)
      maybeBasisDK
  let tagTuples = (fk,,) <$> thisDks <*> subTagIds
  void $ insertTags tagTuples tc
