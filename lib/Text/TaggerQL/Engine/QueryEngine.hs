{-# OPTIONS_GHC -Wno-typed-holes #-}

{- |
Module      : Text.TaggerQL.Engine.Query
Description : This module houses the internal workings of the TaggerQL's query engine.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Engine.QueryEngine (
  queryComplexTerm,
) where

import qualified Data.Foldable as F
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Text.TaggerQL.AST
import Text.TaggerQL.Engine.QueryEngine.Query
import Text.TaggerQL.Engine.QueryEngine.Type

queryComplexTerm :: Bool -> ComplexTerm Text -> QueryReaderT TagKeySet IO FileKeySet
-- For some complex term query a {b {c {d} e {f} g} h{i} j}
--
-- ..{d} where there is no available information about the super term, so no point in
-- querying.
-- This is an unreachable terminal case for all queries.
queryComplexTerm _ (Bottom _) = do
  currentEnv <- asks queryEnv
  lift $ putStrLn $ "Case 1 TS " ++ show currentEnv
  fileSet <- noOperationTerminalEnvironment
  lift $ putStrLn $ "Case 1 FS " ++ show fileSet
  return fileSet
-- The inital query, gathering all tags for the pattern a
queryComplexTerm True (t :<- cts) = do
  currentEnv <- asks queryEnv
  lift $ putStrLn $ "Case 2 TS " ++ show currentEnv
  fileSet <- initializeComplexQueryTermEnv t cts
  lift $ putStrLn $ "Case 2 FS " ++ show fileSet
  return fileSet
-- a{j} the real terminal case, stopping before the unreachable terminal case above.
-- Signifies the end of the rightmost-lowest depth for any given complex term.
queryComplexTerm _ (t :<- ((Bottom bt) :| [])) = do
  tagSetResult <- queryTerms t bt
  currentEnv <- asks queryEnv
  lift $ putStrLn $ "Case 3 TagResult " ++ show tagSetResult
  lift $ putStrLn $ "Case 3 TS " ++ show currentEnv
  let terminalIntersection = IS.intersection currentEnv tagSetResult
  fileSet <-
    asks queryEnvConn
      >>= lift . getFileKeySetFromTagKeySet terminalIntersection
  lift $ putStrLn $ "Case 3 FS " ++ show fileSet
  return fileSet
-- c{d} e{f} ...
-- Signifies the end of the current depth search though there are more
-- file selections that must be queried and intersected to the right.
queryComplexTerm _ (t :<- ((Bottom bt) :| cts)) = do
  tagSetResult <- queryTerms t bt
  currentEnv <- asks queryEnv
  lift $ putStrLn $ "Case 4 TS " ++ show currentEnv
  let terminalTagIntersection = IS.intersection currentEnv tagSetResult
  depthFirstFileSet <-
    asks queryEnvConn
      >>= lift . getFileKeySetFromTagKeySet terminalTagIntersection
  parallelFileResults <- mapM (\ct -> queryComplexTerm False (t :<- (ct :| []))) cts
  let fileSet = intersections $ depthFirstFileSet : parallelFileResults
  lift $ putStrLn $ "Case 4 FS " ++ show fileSet
  return fileSet
-- For non-depth-terminal cases, there is no retrieval of files except from the
-- recursive queries performed below them.
--
-- a{h{..}} representing a depth-first search that is not terminated in the depth
-- but is the right-most term, and therefore no need to intersect its file result.
queryComplexTerm _ (t :<- ((nt :<- nts) :| [])) = do
  tagResult <- queryTerms t nt
  currentEnv <- asks queryEnv
  lift $ putStrLn $ "Case 5 TagResult " ++ show tagResult
  lift $ putStrLn $ "Case 5 TS " ++ show currentEnv
  modifiedEnv <- flip IS.intersection tagResult <$> asks queryEnv
  fileSet <-
    intersections
      . F.toList
      <$> mapM
        ( \ct ->
            local
              (withQueryEnv (const modifiedEnv))
              (queryComplexTerm False (nt :<- (ct :| [])))
        )
        nts
  lift $ putStrLn $ "Case 5 FS " ++ show fileSet
  return fileSet
queryComplexTerm _ (t :<- ((nt :<- nts) :| cts)) = do
  tagResult <- queryTerms t nt
  currentEnv <- asks queryEnv
  lift $ putStrLn $ "Case 6 TS " ++ show currentEnv
  modifiedEnv <- flip IS.intersection tagResult <$> asks queryEnv
  depthFiles <-
    intersections . F.toList
      <$> mapM
        ( \ct ->
            local
              (withQueryEnv (const modifiedEnv))
              (queryComplexTerm False (nt :<- (ct :| [])))
        )
        nts
  widthFiles <-
    intersections . F.toList
      <$> mapM
        (\ct -> queryComplexTerm False (t :<- (ct :| [])))
        cts
  let fileSet = IS.intersection depthFiles widthFiles
  lift $ putStrLn $ "Case 6 FS " ++ show fileSet
  return fileSet

noOperationTerminalEnvironment :: QueryReaderT TagKeySet IO FileKeySet
noOperationTerminalEnvironment = return IS.empty

initializeComplexQueryTermEnv ::
  Traversable t =>
  Term Text ->
  t (ComplexTerm Text) ->
  QueryReaderT TagKeySet IO FileKeySet
initializeComplexQueryTermEnv t cts = do
  initEnv <- queryTerm t
  resultFiles <-
    mapM
      ( \ct ->
          local
            (withQueryEnv (const initEnv))
            (queryComplexTerm False (t :<- (ct :| [])))
      )
      cts
  let fileSet = intersections . F.toList $ resultFiles
  return fileSet

intersections :: [IS.IntSet] -> IS.IntSet
intersections [] = IS.empty
intersections xs = L.foldl1' IS.intersection xs