{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interface.Handler.WidgetQueryRequest (
  WidgetQueryRequest,
  WidgetSentenceBranch,
  squashWidgetQueryRequest,
  moveQueryWidgetNodeTo,
  deleteWidgetQueryNode,
  appendWidgetQueryNode,
  createWidgetSentenceBranch,
  queryWidgetQueryRequestNodes,
) where

import Data.Foldable (toList)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, deleteAt, elemIndexL, insertAt, lookup, (|>))
import Data.Tagger (SetOp)
import Data.Text (Text)
import Database.Tagger.Type (File, TaggedConnection)
import Text.TaggerQL
import Text.TaggerQL.AST (
  Request (Request),
  SentenceTree (SentenceBranch),
 )
import Prelude hiding (lookup)

{- |
 A newtype to be used in lieu of the 'Request` type. This type will be converted
 to a 'Request` just before a query is executed. This type is used for easier editing.
-}
newtype WidgetQueryRequest a = WidgetQueryRequest {widgetRequest :: Seq (SentenceTree a)}
  deriving (Show, Eq, Functor)

{- |
 newtype to illustrate the fact that any 'Request` made by the widget in the tagger UI
 can actually be squashed to a 'SentenceBranch` constructed SentenceTree provided
 since the model also provides a 'SetOp`.
-}
newtype WidgetSentenceBranch a = WidgetSentenceBranch (SentenceTree a)
  deriving (Show, Eq, Functor)

squashWidgetQueryRequest :: WidgetQueryRequest a -> Request a
squashWidgetQueryRequest (WidgetQueryRequest sts) = Request . toList $ sts

moveQueryWidgetNodeTo ::
  Eq a =>
  Int ->
  Int ->
  WidgetQueryRequest a ->
  WidgetQueryRequest a
moveQueryWidgetNodeTo from to wr@(WidgetQueryRequest sts) = fromMaybe wr $ do
  nodeToMove <- lookup from sts
  nodeInGivenDestination <- lookup to sts
  let removedToNode = deleteAt from sts
  destinationNodeIx <- elemIndexL nodeInGivenDestination removedToNode
  return . WidgetQueryRequest $ insertAt destinationNodeIx nodeToMove removedToNode

deleteWidgetQueryNode :: Int -> WidgetQueryRequest a -> WidgetQueryRequest a
deleteWidgetQueryNode n (WidgetQueryRequest sts) = WidgetQueryRequest $ deleteAt n sts

appendWidgetQueryNode ::
  WidgetSentenceBranch a ->
  WidgetQueryRequest a ->
  WidgetQueryRequest a
appendWidgetQueryNode (WidgetSentenceBranch st) (WidgetQueryRequest sts) =
  WidgetQueryRequest $ sts |> st

createWidgetSentenceBranch :: SetOp -> Request a -> WidgetSentenceBranch a
createWidgetSentenceBranch so (Request ss) = WidgetSentenceBranch $ SentenceBranch so ss

queryWidgetQueryRequestNodes ::
  TaggedConnection ->
  WidgetQueryRequest Text ->
  IO (Seq (HashSet File))
queryWidgetQueryRequestNodes tc (WidgetQueryRequest sts) =
  traverse
    (queryWidgetSentenceBranch tc . WidgetSentenceBranch)
    sts

queryWidgetSentenceBranch ::
  TaggedConnection ->
  WidgetSentenceBranch Text ->
  IO (HashSet File)
queryWidgetSentenceBranch tc (WidgetSentenceBranch st) =
  combinableSentenceResultSet <$> queryRequest tc (Request [st])