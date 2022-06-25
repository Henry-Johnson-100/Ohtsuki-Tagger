{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}

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

import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Tagger
import qualified Data.Text as T
import Database.Tagger
import Text.TaggerQL.AST

data CombinableSentenceResult
  = CombinableSentenceResult SetOp (HashSet.HashSet File)
  deriving (Show, Eq)

combinableSentenceResultSetOp :: CombinableSentenceResult -> SetOp
combinableSentenceResultSetOp (CombinableSentenceResult so _) = so

combinableSentenceResultSet :: CombinableSentenceResult -> HashSet.HashSet File
combinableSentenceResultSet (CombinableSentenceResult _ s) = s

newtype TermResult = TermResult (HashSet.HashSet File) deriving (Show, Eq)

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

combineSentences :: [CombinableSentenceResult] -> CombinableSentenceResult
combineSentences [] = emptyCombinableSentenceResult
combineSentences cs = L.foldl1' combineSentence cs

{- |
 Return an empty result with a default 'SetOp` of Union.
-}
emptyCombinableSentenceResult :: CombinableSentenceResult
emptyCombinableSentenceResult = undefined

{- |
 Left-associative combination of the results of a combinable sentence query.
-}
combineSentence ::
  CombinableSentenceResult ->
  CombinableSentenceResult ->
  CombinableSentenceResult
combineSentence x y = undefined

queryCombinableSentence ::
  TaggedConnection ->
  CombinableSentence T.Text ->
  IO CombinableSentenceResult
queryCombinableSentence tc cs = undefined

querySentence ::
  TaggedConnection ->
  Sentence T.Text ->
  IO TermResult
querySentence tc s = undefined

queryTermTree ::
  TaggedConnection ->
  TermTree T.Text ->
  IO TermResult
queryTermTree tc tt = undefined

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
querySimpleTerm tc (SimpleTerm tt) = undefined

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
