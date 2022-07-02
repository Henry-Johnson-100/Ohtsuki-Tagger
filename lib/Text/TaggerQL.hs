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

import Control.Monad (void, (<=<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
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
  queryForFileByDescriptorSubTagDescriptor,
  queryForFileByDescriptorSubTagMetaDescriptor,
  queryForFileByFilePatternAndDescriptor,
  queryForFileByFilePatternAndFilePattern,
  queryForFileByFilePatternAndMetaDescriptor,
  queryForFileByFilePatternAndUntagged,
  queryForFileByMetaDescriptorSubTagDescriptor,
  queryForFileByMetaDescriptorSubTagMetaDescriptor,
  queryForFileByPattern,
  queryForTagByFileAndDescriptorKey,
  queryForTagByFileAndDescriptorKeyAndNullSubTagOf,
  queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf,
  queryForTagBySubTagTriple,
  queryForUntaggedFiles,
 )
import Database.Tagger.Type (
  Descriptor (descriptorId),
  File,
  RecordKey,
  Tag (tagId),
  TaggedConnection,
 )
import System.IO (hPrint, stderr)
import Tagger.Util (catMaybeTM, hoistMaybe)
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
    <$> catMaybeTM (queryTermTree tc) tts

{- |
 Return 'Nothing` if the given 'TermTree` is Complex and Bottom.
-}
queryTermTree ::
  TaggedConnection ->
  TermTree Text ->
  MaybeT IO TermResult
queryTermTree tc tt =
  case tt of
    Simple st -> lift $ querySimpleTerm tc st
    Complex ct -> queryComplexTerm tc ct

{- |
 A depth-first query with 'ComplexTerm`s.
 Each 'Term` in a given 'ComplexTerm` will first find its relation
 query with its immediate sub-term then intersect that result with the results
 of its sub-term's sub-terms. It will do this until it reaches a term-bottom relation
 where it will perform one final query then move on to query relations of the term
 and any sub-terms adjacent to the first.

 This is a recursive, depth-first tree traversal where the results of the top relation
 are intersected with the intersected results of every query below it.
-}
queryComplexTerm ::
  TaggedConnection ->
  ComplexTerm Text ->
  MaybeT IO TermResult
-- This match should never be called, but if it is, it is filtered by querySentence.
queryComplexTerm _ (Bottom _) = hoistMaybe Nothing
-- The bottom of a depth-relation query, will perform a single query on the current
-- term and bottom subterm then move to any adjacent subterms
--
-- This match contains the terminal case in t :<- (Bottom bst :| [])
queryComplexTerm tc (t :<- ((Bottom bst) :| sts)) =
  if null sts
    then queryComplexTermRelation tc (TermTag t) (TermSubTag bst)
    else do
      thisResult <- queryComplexTermRelation tc (TermTag t) (TermSubTag bst)
      nextResults <-
        lift $
          intersectTermResults
            <$> catMaybeTM (\ct' -> queryComplexTerm tc (t :<- [ct'])) sts
      let combinedResults = intersectTermResult thisResult nextResults
      return combinedResults
-- A relation query that has not reached the bottom yet.
-- Will intersect the result of the current term and next subterm with the results
-- of the subterm and subterm's subterms before moving on to adjacent subterms.
queryComplexTerm tc (t :<- (st :| sts)) =
  if null sts
    then do
      thisResult <-
        queryComplexTermRelation tc (TermTag t) (TermSubTag . complexTermNode $ st)
      nestedResult <- queryComplexTerm tc st
      let combinedResults = intersectTermResult thisResult nestedResult
      return combinedResults
    else do
      thisResult <-
        queryComplexTermRelation tc (TermTag t) (TermSubTag . complexTermNode $ st)
      nestedResult <- queryComplexTerm tc st
      nextResults <-
        lift $
          intersectTermResults
            <$> catMaybeTM (\ct' -> queryComplexTerm tc (t :<- [ct'])) sts
      let combinedResults =
            intersectTermResults [thisResult, nestedResult, nextResults]
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
    Simple t -> insertSimpleTerm tc t
    Complex t -> void $ insertComplexTerm tc True [] t

insertComplexTerm ::
  TaggedConnection ->
  Bool ->
  [RecordKey Tag] ->
  ComplexTerm (RecordKey File, Text) ->
  IO [RecordKey Tag]
insertComplexTerm tc True basisTagList ct@(complexTermNode -> t@(Term _ (fk, dp))) = do
  putStrLn $ "In match 1: inserting" ++ show t
  -- Insert all necessary top-level descriptors
  void $ insertTerm tc t
  -- Collect all top-level tags that match the given pattern,
  -- including the tags just inserted
  newBasisTagList <-
    map tagId
      <$> queryForTagByFileKeyAndDescriptorPatternAndNullSubTagOf fk dp tc
  putStrLn $ "In match 1: new basis list: " ++ show newBasisTagList
  -- proceed to the tree traversal with the list of top-level tags.
  insertComplexTerm tc False (newBasisTagList `L.union` basisTagList) ct
insertComplexTerm _ _ _ (Bottom _) = do
  putStrLn "In match 2: Bottom"
  mempty
insertComplexTerm tc _ basisTagList (t :<- ((Bottom bst) :| sts)) =
  if null sts
    then do
      putStrLn $ "In match 3.1: inserting" ++ show t ++ " | " ++ show bst
      void $ insertComplexTermRelation tc basisTagList t bst
      putStrLn $ "Returning basis: " ++ show basisTagList
      return basisTagList
    else do
      putStrLn $ "In match 3.2: inserting" ++ show t ++ " | " ++ show bst
      void $ insertComplexTermRelation tc basisTagList t bst
      putStrLn $ "mapping over adjacencies: " ++ show sts
      mapM_ (\ct' -> insertComplexTerm tc False basisTagList (t :<- [ct'])) sts
      putStrLn $ "Return basis" ++ show basisTagList
      return basisTagList
insertComplexTerm tc _ basisTagList (t :<- (st :| sts)) =
  if null sts
    then do
      putStrLn $ "In match 4.1: inserting" ++ show t ++ " | " ++ (show . complexTermNode) st
      nextLevelOfTags <- insertComplexTermRelation tc basisTagList t (complexTermNode st)
      putStrLn $ "next tag basis level" ++ show nextLevelOfTags
      void $ insertComplexTerm tc False nextLevelOfTags st
      return basisTagList
    else do
      putStrLn $ "in match 4.2: inserting" ++ show t ++ " | " ++ (show . complexTermNode) st
      nextLevelOfTags <- insertComplexTermRelation tc basisTagList t (complexTermNode st)
      putStrLn $ "next tag basis level" ++ show nextLevelOfTags
      void $ insertComplexTerm tc False nextLevelOfTags st
      putStrLn $ "mapping of adjacencies" ++ show sts
      mapM_ (\ct' -> insertComplexTerm tc False basisTagList (t :<- [ct'])) sts
      return basisTagList

insertComplexTermRelation ::
  TaggedConnection ->
  [RecordKey Tag] ->
  Term (RecordKey File, Text) ->
  Term (RecordKey File, Text) ->
  IO [RecordKey Tag]
insertComplexTermRelation tc prevBasisTagList basis@(Term _ (fk, basisP)) subtag@(Term _ (_, subTagP)) = do
  putStrLn $ "In ICTR: Recieved basis " ++ show prevBasisTagList
  putStrLn $ "For: " ++ show basis ++ " | " ++ show subtag
  -- basisDescriptors <- queryForDescriptorByPattern basisP tc
  -- let basisSuperTagTriples = (fk,,) <$> (descriptorId <$> basisDescriptors) <*> prevBasisTagList
  -- basisSuperTags <- unions <$> mapM (`queryForTagBySubTagTriple` tc) basisSuperTagTriples
  -- putStrLn $ "Found new basis super tags" ++ show basisSuperTags
  subTagDescriptors <- queryForDescriptorByPattern subTagP tc
  putStrLn $ "Found subtag descriptors: " ++ show subTagDescriptors
  let newTagTriples = (fk,,) <$> (descriptorId <$> subTagDescriptors) <*> (Just <$> prevBasisTagList)
  newBasis <- insertTags newTagTriples tc
  allCollectedNewSubtags <- do
    existingTags <- map tagId . unions <$> mapM (`queryForTagBySubTagTriple` tc) ((fk,,) <$> (descriptorId <$> subTagDescriptors) <*> prevBasisTagList)
    return $ L.union newBasis existingTags
  putStrLn $ "Collected all subtags: " ++ show allCollectedNewSubtags
  return allCollectedNewSubtags
  -- putStrLn $ "found new basis: " ++ show newBasis
  -- return newBasis

{- |
 Insert descriptors that match a text in the given simple term as top-level
 tags.
-}
insertSimpleTerm :: TaggedConnection -> SimpleTerm (RecordKey File, Text) -> IO ()
insertSimpleTerm tc (SimpleTerm t) =
  void $ insertTerm tc t

insertTerm :: TaggedConnection -> Term (RecordKey File, Text) -> IO [RecordKey Tag]
insertTerm tc (Term _ (fk, dp)) = do
  matchingDescriptors <- queryForDescriptorByPattern dp tc
  let tagTriples = (fk,,Nothing) . descriptorId <$> matchingDescriptors
  insertTags tagTriples tc

unions :: (Foldable t, Eq a) => t [a] -> [a]
unions xs = if null xs then [] else L.foldl' L.union [] xs

-- {- |
--  See 'queryComplexTerm` for an example of why this function's pattern matches
--  are structured this way. It is the same type of tree traversal.
-- -}
-- insertComplexTerm ::
--   TaggedConnection ->
--   -- | Boolean switch for when the inserter is at the top-level.
--   -- This is required because each lower-level insertion is operating on the assumption
--   -- that the tags they are subtagging already exist.
--   -- For the z+1st layer, this is not true without first inserting every possible
--   -- tag for the given descriptor at the zth level.
--   --
--   -- Therefore this boolean, tells the inserter it is currently at the z=0th level
--   -- and to insert every tag then just move to the z=1st layer.
--   Bool ->
--   ComplexTerm (RecordKey File, Text) ->
--   IO ()
-- -- this match creates top-level terms, which are necessary for traversing the tree
-- -- and adding sub-terms.
-- --
-- -- It blanket adds all 'Descriptor`s descripbed by the term, since the underlying call
-- -- to 'insertTags` will ignore any top-level duplicates.
-- insertComplexTerm tc True ct@(complexTermNode -> t) = do
--   insertTerm tc t
--   insertComplexTerm tc False ct
-- -- Do nothing in the Bottom case, since there is not enough information to
-- -- insert a tag, the appopriate tag for this given Bottom should have been created
-- -- by the insertComplexTerm call one level above this one.
-- insertComplexTerm _ _ (Bottom _) = mempty
-- insertComplexTerm tc _ (t :<- ((Bottom bst) :| sts)) =
--   if null sts
--     then -- terminal case for when the inserter has reached the bottom of a branch.
--       insertComplexTermRelation tc t bst
--     else -- terminal case for when the insert has reach the bottom of the right-most
--     -- branch in the term tree.
--     do
--       -- insert current child as subterm
--       insertComplexTermRelation tc t bst
--       -- map over and insert every adjacent child.
--       mapM_ (\ct' -> insertComplexTerm tc False (t :<- [ct'])) sts
-- insertComplexTerm tc _ (t :<- (st :| sts)) =
--   if null sts
--     then do
--       -- insert current child
--       insertComplexTermRelation tc t (complexTermNode st)
--       -- insert nested children as not-top-level terms
--       insertComplexTerm tc False st
--     else do
--       -- insert current child
--       insertComplexTermRelation tc t (complexTermNode st)
--       -- insert nested children as not-top-level terms
--       insertComplexTerm tc False st
--       -- map over adjacent children after the nested subterms are inserted.
--       mapM_ (\ct' -> insertComplexTerm tc False (t :<- [ct'])) sts

-- -- operates on the sub term so top-level tags need to be created before calling this.
-- insertComplexTermRelation ::
--   TaggedConnection ->
--   Term (RecordKey File, Text) ->
--   Term (RecordKey File, Text) ->
--   IO ()
-- insertComplexTermRelation tc (Term _ (fk, basisP)) (Term _ (_, subTagP)) = do
--   basisDescriptors <- queryForDescriptorByPattern basisP tc
--   basisTags <-
--     unions
--       <$> mapM
--         (\dk -> queryForTagByFileAndDescriptorKey fk (descriptorId dk) tc)
--         basisDescriptors
--   subTagDescriptors <- queryForDescriptorByPattern subTagP tc
--   let tagTriples =
--         (fk,,)
--           <$> (descriptorId <$> subTagDescriptors) <*> (Just . tagId <$> basisTags)
--   insertTags tagTriples tc
--  where
--   unions xs = case xs of [] -> []; _ -> L.foldl' L.union [] xs
