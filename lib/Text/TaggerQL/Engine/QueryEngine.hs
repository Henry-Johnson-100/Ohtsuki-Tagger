{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
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
import qualified Data.List.NonEmpty as NE
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
import Text.TaggerQL.AST (
  ComplexTerm (..),
  Request (Request),
  Sentence (Sentence),
  SentenceSet (..),
  SentenceTree (..),
  SimpleTerm (..),
  Term (Term),
  TermTree (..),
 )
import Text.TaggerQL.Engine.QueryEngine.Query (
  QueryEnv (QueryEnv, envSuperTerm, envTagSet),
  Sub (..),
  Super (..),
  getFileSetFromTagSet,
  queryTerm,
  queryTerms,
 )
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

newtype TermResult = TermResult {runTermResult :: HashSet File}
  deriving (Show, Eq, Semigroup, Monoid)

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
querySentenceSet tc (SentenceSet so s) =
  CombinableSentenceResult so . runTermResult <$> querySentence tc s

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
    Complex ct -> queryComplexTerm tc ct

queryComplexTerm :: TaggedConnection -> ComplexTerm Text -> IO TermResult
queryComplexTerm _ (Bottom _) = return mempty
queryComplexTerm tc (t :<- nestedTerms) = flip runReaderT (QueryEnv mempty t tc) $ do
  -- The same term as t, but using the reader's for the sake of clarity.
  givenSuperTerm <- asks envSuperTerm
  initialTagSet <- Super <$> queryTerm givenSuperTerm
  queryResult <-
    local (\e -> e{envTagSet = initialTagSet})
      . queryParallelSubTerms
      $ nestedTerms
  TermResult <$> (getFileSetFromTagSet . runSuper $ queryResult)

{- |
 width-wise query
-}
queryParallelSubTerms ::
  NonEmpty (ComplexTerm Text) ->
  ReaderT QueryEnv IO (Super (HashSet Tag))
queryParallelSubTerms (widthTerminal :| []) = queryNestedSubTerm widthTerminal
queryParallelSubTerms (term :| parallelTerms) = do
  (runSuper -> currentTermQueryResults) <- queryNestedSubTerm term
  (runSuper -> parallelResults) <- queryParallelSubTerms . NE.fromList $ parallelTerms
  return . Super $
    HS.intersection
      currentTermQueryResults
      parallelResults

{- |
 depth-wise query
-}
queryNestedSubTerm :: ComplexTerm Text -> ReaderT QueryEnv IO (Super (HashSet Tag))
queryNestedSubTerm (Bottom t) = do
  currentTermResult <- runSubQuery t
  currentSuperSet <- asks envTagSet
  return . joinOnSuperTag currentSuperSet $ currentTermResult
queryNestedSubTerm (t :<- nestedTerms) = do
  subQueryResults <- do
    currentTermResults <- runSubQuery t
    Sub . runSuper
      <$> ( local
              ( \e ->
                  e
                    { envTagSet = Super . runSub $ currentTermResults
                    , envSuperTerm = t
                    }
              )
              . queryParallelSubTerms
              $ nestedTerms
          )
  currentSuperSet <- asks envTagSet
  return . joinOnSuperTag currentSuperSet $ subQueryResults

runSubQuery :: Term Text -> ReaderT QueryEnv IO (Sub (HashSet Tag))
runSubQuery t = do
  superTerm <- asks envSuperTerm
  queryTerms superTerm t

joinOnSuperTag :: Super (HashSet Tag) -> Sub (HashSet Tag) -> Super (HashSet Tag)
joinOnSuperTag (runSuper -> superSet) (runSub -> subSet) =
  let !subTagOfIdSet = HS.map tagSubtagOfId subSet
   in Super . HS.filter (flip HS.member subTagOfIdSet . Just . tagId) $ superSet

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
