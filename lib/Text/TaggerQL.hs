{-# HLINT ignore "Use concatMap" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL
Description : The front-end and interface for running TaggerQL queries on a Tagger database.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL (
  CombinableSentenceResult,
  combinableSentenceResultSetOp,
  combinableSentenceResultSet,
  runRequest,
  queryRequest,
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.HashSet as HS
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Tagger
import qualified Data.Text as T
import Database.Tagger
import Tagger.Util
import Text.TaggerQL.AST

data CombinableSentenceResult
  = CombinableSentenceResult SetOp (HashSet.HashSet File)
  deriving (Show, Eq)

combinableSentenceResultSetOp :: CombinableSentenceResult -> SetOp
combinableSentenceResultSetOp (CombinableSentenceResult so _) = so

combinableSentenceResultSet :: CombinableSentenceResult -> HashSet.HashSet File
combinableSentenceResultSet (CombinableSentenceResult _ s) = s

newtype TermResult = TermResult {termResult :: HashSet.HashSet File} deriving (Show, Eq)

newtype SimpleTerm = SimpleTerm (TermTree T.Text)
  deriving (Show, Eq)

newtype TermTag = TermTag (TermTree T.Text) deriving (Show, Eq)

newtype TermSubTag = TermSubTag (TermTree T.Text) deriving (Show, Eq)

runRequest ::
  TaggedConnection ->
  Request T.Text ->
  IO (HashSet.HashSet File)
runRequest tc r = combinableSentenceResultSet <$> queryRequest tc r

queryRequest ::
  TaggedConnection ->
  Request T.Text ->
  IO CombinableSentenceResult
queryRequest tc (Request cs) = combineSentences <$> mapM (queryCombinableSentence tc) cs

queryCombinableSentence ::
  TaggedConnection ->
  CombinableSentence T.Text ->
  IO CombinableSentenceResult
queryCombinableSentence tc cs = undefined

querySentence ::
  TaggedConnection ->
  Sentence T.Text ->
  IO TermResult
querySentence tc (Sentence tts) =
  intersectTermResults . catMaybes <$> mapM (runMaybeT . queryTermTree tc) tts

{- |
 Return 'Nothing` if the given tree is 'Bottom`,
 that way, these terms can be filtered before combination instead of trying to do things
 like intersect an empty bottom set.
-}
queryTermTree ::
  TaggedConnection ->
  TermTree T.Text ->
  MaybeT IO TermResult
queryTermTree tc tt =
  case tt of
    Simple _ -> lift . querySimpleTerm tc . SimpleTerm $ tt
    Bottom _ -> hoistMaybe Nothing
    t :<- ts -> lift undefined

queryTermRelation ::
  TaggedConnection ->
  TermTag ->
  TermSubTag ->
  IO TermResult
queryTermRelation
  tc
  (TermTag t)
  (TermSubTag st) = undefined

querySimpleTerm ::
  TaggedConnection ->
  SimpleTerm ->
  IO TermResult
querySimpleTerm tc (SimpleTerm (termTreeNode -> Term qc p)) =
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
intersectTermResults trs = TermResult . L.foldl1' HS.intersection . map termResult $ trs

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
emptyCombinableSentenceResult = undefined

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