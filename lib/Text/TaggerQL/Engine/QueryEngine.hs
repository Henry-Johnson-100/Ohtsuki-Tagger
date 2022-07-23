{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

{- |
Module      : Text.TaggerQL.Engine.Query
Description : This module houses the internal workings of the TaggerQL's query engine.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Engine.QueryEngine (
  queryComplexTermTopLevel,
) where

import Control.Monad.Trans.Reader
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Database.Tagger.Type
import Text.TaggerQL.AST
import Text.TaggerQL.Engine.QueryEngine.Query
import Text.TaggerQL.Engine.QueryEngine.Type

queryComplexTermTopLevel :: ComplexTerm Text -> QueryReader (HashSet File)
queryComplexTermTopLevel (Bottom _) = return mempty
queryComplexTermTopLevel ct@(t :<- _) = do
  topLevelTagSet <- queryTerm t
  local (\e -> e{envTagSet = topLevelTagSet}) $ queryComplexTerm ct
 where
  queryComplexTerm :: ComplexTerm Text -> QueryReader (HashSet File)
  queryComplexTerm (Bottom _) = return mempty
  queryComplexTerm (currentTerm :<- ((Bottom bottomTerminal) :| parallelSubTerms)) = do
    terminalTagSet <- queryTerms currentTerm bottomTerminal
    terminalFileSet <-
      asks envTagSet
        >>= getFileSetFromTagSet . flip joinTagSet terminalTagSet
    case parallelSubTerms of
      [] -> return terminalFileSet
      _ -> do
        parallelFileSets <-
          mapM
            (\pt -> queryComplexTerm (currentTerm :<- (pt :| [])))
            parallelSubTerms
        return . intersections $ terminalFileSet : parallelFileSets
  queryComplexTerm
    ( currentTerm
        :<- ( nestedComplexTerm@(firstSubTerm :<- _)
                :| parallelSubTerms
              )
      ) = do
      tagSet <- queryTerms currentTerm firstSubTerm
      newTagSet <- asks envTagSet >>= return . flip joinTagSet tagSet
      depthFirstFileSet <-
        local (\e -> e{envTagSet = newTagSet}) $
          queryComplexTerm nestedComplexTerm
      case parallelSubTerms of
        [] -> return depthFirstFileSet
        _ -> do
          parallelFileSets <-
            mapM
              (\pt -> queryComplexTerm (currentTerm :<- (pt :| [])))
              parallelSubTerms
          return . intersections $ depthFirstFileSet : parallelFileSets

intersections :: Hashable a => [HashSet a] -> HashSet a
intersections [] = HS.empty
intersections xs = L.foldl1' HS.intersection xs