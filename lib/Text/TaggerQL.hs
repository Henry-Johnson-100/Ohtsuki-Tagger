{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}

{- |
Module      : Text.TaggerQL
Description : The front-end and interface for running TaggerQL queries on a Tagger database.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL (
  runTaggerQL,
) where

import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.List.NonEmpty as NE
import Data.Tagger
import qualified Data.Text as T
import Database.Tagger
import Text.TaggerQL.AST

data TermResult = TermResult SetOp (HashSet.HashSet File) deriving (Show, Eq)

{- |
 Given a TaggerQL query, produce a set of the 'File`s it corresponds to.
-}
runTaggerQL :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
runTaggerQL = undefined

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

-- foldTermResults :: NE.NonEmpty TermResult -> TermResult
-- foldTermResults (t NE.:| []) = t
-- foldTermResults (t NE.:| (t' : ts)) = foldTermResults (combineTermResults t t' NE.:| ts)

-- desugarTerm :: Term T.Text -> [DesugaredTerm T.Text]
-- desugarTerm t@(Term so qc b ps) =
--   case ps of
--     [] -> []
--     _ ->
--       let removePredicates t' = t'{termPredicate = []}
--           basisPreds =
--             map
--               (Term so qc b . (: []) . removePredicates)
--               ps
--           desugaredPreds = map unsugar . concat $ map desugarTerm ps
--        in DesugaredTerm <$> (basisPreds ++ desugaredPreds)

-- {- |
--  A right-associative combination of the results of a TaggerQL query.

--  Preserves the left-most 'SetOp` but consumes the right-most.
-- -}
-- combineTermResults :: TermResult -> TermResult -> TermResult
-- combineTermResults (TermResult sox tx) (TermResult soy ty) =
--   TermResult sox $ combFun soy tx ty

-- queryTerm :: TaggedConnection -> Term T.Text -> IO TermResult
-- queryTerm tc t@(Term _ _ _ predicates) =
--   if null predicates
--     then queryNullPredicateTerm tc . DesugaredTerm $ t
--     else queryWithPredicates tc t

-- queryWithPredicates :: TaggedConnection -> Term T.Text -> IO TermResult
-- queryWithPredicates tc (Term so qc t predicates) = undefined

-- {- |
--  Dispatch a basis and predicate pattern to the appropriate query based on the
--  'QueryCriteria` of both.
-- -}
-- getSubQueryType ::
--   QueryCriteria ->
--   QueryCriteria ->
--   T.Text ->
--   T.Text ->
--   TaggedConnection ->
--   IO (HashSet.HashSet File)
-- getSubQueryType bc pc bp pp tc = undefined

-- queryNullPredicateTerm :: TaggedConnection -> DesugaredTerm T.Text -> IO TermResult
-- queryNullPredicateTerm tc (DesugaredTerm (Term so qc t _)) =
--   case qc of
--     DescriptorCriteria ->
--       TermResult so . HashSet.fromList
--         <$> flatQueryForFileByTagDescriptorPattern t tc
--     MetaDescriptorCriteria -> do
--       ds <- queryForDescriptorByPattern t tc
--       results <-
--         HashSet.fromList
--           <$> mapMConcat (flip flatQueryForFileOnMetaRelation tc . descriptorId) ds
--       return $ TermResult so results
--     FilePatternCriteria -> TermResult so . HashSet.fromList <$> queryForFileByPattern t tc
--     UntaggedCriteria -> TermResult so . HashSet.fromList <$> queryForUntaggedFiles tc

-- mapMConcat :: (Traversable t, Monad f) => (a1 -> f [a2]) -> t a1 -> f [a2]
-- mapMConcat f = fmap concat . mapM f