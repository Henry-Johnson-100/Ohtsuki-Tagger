{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Module      : Text.TaggerQL.Engine.Query
Description : This module houses the internal workings of the TaggerQL's query engine.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Engine.QueryEngine (
  TaggerQLQuery (..),
  taggerQL,
  queryRequest,
  CombinableSentenceResult (..),
  combinableSentenceResultSetOp,
  combinableSentenceResultSet,
) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
  asks,
  local,
 )
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
  queryForFileByPattern,
  queryForUntaggedFiles,
 )
import Database.Tagger.Type (File, Tag (..), TaggedConnection)
import System.IO (hPrint, stderr)
import Text.TaggerQL.AST
import Text.TaggerQL.Engine.QueryEngine.Query
import Text.TaggerQL.Engine.QueryEngine.Type
import Text.TaggerQL.Parser.Internal (parse, requestParser)

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

newtype TermResult = TermResult {termResult :: HashSet File} deriving (Show, Eq)

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
    Complex ct -> undefined

-- TermResult
--   <$> runReaderT (queryComplexTermTopLevel ct) (QueryEnv mempty tc)

queryComplexTermTopLevel = undefined

-- queryComplexTermTopLevel :: ComplexTerm Text -> QueryReader (HashSet File)
-- queryComplexTermTopLevel (Bottom _) = return mempty
-- queryComplexTermTopLevel ct@(t :<- _) = do
--   topLevelTagSet <- queryTerm t
--   local (\e -> e{envTagSet = topLevelTagSet}) $ queryComplexTerm ct
--  where
--   queryComplexTerm :: ComplexTerm Text -> QueryReader (HashSet File)
--   queryComplexTerm (Bottom _) = return mempty
--   queryComplexTerm (currentTerm :<- ((Bottom bottomTerminal) :| parallelSubTerms)) = do
--     terminalTagSet <- queryTerms currentTerm bottomTerminal
--     terminalFileSet <-
--       asks envTagSet
--         >>= getFileSetFromTagSet . HS.map subTag
--           . flip joinTagSet (HS.map SubTag terminalTagSet)
--           . HS.map SuperTag
--     case parallelSubTerms of
--       [] -> return terminalFileSet
--       _ -> do
--         parallelFileSets <-
--           mapM
--             (\pt -> queryComplexTerm (currentTerm :<- (pt :| [])))
--             parallelSubTerms
--         return . intersections $ terminalFileSet : parallelFileSets
--   queryComplexTerm
--     ( currentTerm
--         :<- ( nestedComplexTerm@(firstSubTerm :<- _)
--                 :| parallelSubTerms
--               )
--       ) = do
--       tagSet <- queryTerms currentTerm firstSubTerm
--       newTagSet <-
--         asks envTagSet
--           >>= return
--             . flip joinTagSet (HS.map SubTag tagSet)
--             . HS.map SuperTag
--       depthFirstFileSet <-
--         local (\e -> e{envTagSet = HS.map subTag newTagSet}) $
--           queryComplexTerm nestedComplexTerm
--       case parallelSubTerms of
--         [] -> return depthFirstFileSet
--         _ -> do
--           parallelFileSets <-
--             mapM
--               (\pt -> queryComplexTerm (currentTerm :<- (pt :| [])))
--               parallelSubTerms
--           return . intersections $ depthFirstFileSet : parallelFileSets

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
 Given two HashSets of 'Tag`s, intersect them, such that only 'Tag`s in the second set
 that are subtags of 'Tag`s in the first remain.

 like:

 @
  SELECT DISTINCT t2.*

  FROM (...) as t1

  JOIN (...) as t2

    ON t1.id = t2.subTagOfId
 @
-}
joinTagSet :: HashSet SuperTag -> HashSet SubTag -> HashSet SubTag
joinTagSet (HS.map superTag -> superSet) (HS.map subTag -> subSet) =
  let !superIdSet = HS.map tagId superSet
   in HS.map SubTag $
        HS.filter
          (\(tagSubtagOfId -> mstid) -> maybe False (`HS.member` superIdSet) mstid)
          subSet

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

intersections :: Hashable a => [HashSet a] -> HashSet a
intersections [] = HS.empty
intersections xs = L.foldl1' HS.intersection xs