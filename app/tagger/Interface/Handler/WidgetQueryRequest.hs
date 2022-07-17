{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Interface.Handler.WidgetQueryRequest (
  WidgetQueryRequest (widgetQueryRequest),
  WidgetSentenceBranch (
    widgetSentenceBranchText,
    widgetSentenceBranch,
    widgetSentenceBranchCount
  ),
  widgetSentenceBranchSetOp,
  emptyWidgetQueryRequest,
  squashWidgetQueryRequest,
  moveQueryWidgetNodeTo,
  deleteWidgetQueryNode,
  appendWidgetQueryNode,
  createWidgetSentenceBranch,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, withExceptT)
import Data.Foldable (toList)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, deleteAt, elemIndexL, empty, insertAt, lookup, (|>))
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import Database.Tagger.Type (TaggedConnection)
import Text.TaggerQL (combinableSentenceResultSet, queryRequest)
import Text.TaggerQL.AST (
  Request (Request),
  SentenceTree (..),
 )
import Text.TaggerQL.Parser.Internal (parse, requestParser)
import Prelude hiding (lookup)

newtype WidgetQueryRequest = WidgetQueryRequest
  { widgetQueryRequest :: Seq WidgetSentenceBranch
  }
  deriving (Show, Eq)

data WidgetSentenceBranch = WidgetSentenceBranch
  { widgetSentenceBranchText :: Text
  , widgetSentenceBranch :: SentenceTree Text
  , widgetSentenceBranchCount :: Int
  }
  deriving (Show, Eq)

widgetSentenceBranchSetOp :: WidgetSentenceBranch -> SetOp
widgetSentenceBranchSetOp (widgetSentenceBranch -> st) =
  case st of
    SentenceBranch so _ -> so
    SentenceNode _ -> Union

emptyWidgetQueryRequest :: WidgetQueryRequest
emptyWidgetQueryRequest = WidgetQueryRequest empty

squashWidgetQueryRequest :: WidgetQueryRequest -> Request Text
squashWidgetQueryRequest (WidgetQueryRequest sts) =
  Request . toList $
    (widgetSentenceBranch <$> sts)

moveQueryWidgetNodeTo ::
  Int ->
  Int ->
  WidgetQueryRequest ->
  WidgetQueryRequest
moveQueryWidgetNodeTo from to wr@(WidgetQueryRequest sts) = fromMaybe wr $ do
  nodeToMove <- lookup from sts
  nodeInGivenDestination <- lookup to sts
  let removedToNode = deleteAt from sts
  destinationNodeIx <- elemIndexL nodeInGivenDestination removedToNode
  return . WidgetQueryRequest $ insertAt destinationNodeIx nodeToMove removedToNode

deleteWidgetQueryNode :: Int -> WidgetQueryRequest -> WidgetQueryRequest
deleteWidgetQueryNode n (WidgetQueryRequest sts) = WidgetQueryRequest $ deleteAt n sts

appendWidgetQueryNode ::
  WidgetSentenceBranch ->
  WidgetQueryRequest ->
  WidgetQueryRequest
appendWidgetQueryNode wsb (WidgetQueryRequest sts) =
  WidgetQueryRequest $ sts |> wsb

createWidgetSentenceBranch ::
  TaggedConnection ->
  SetOp ->
  Text ->
  ExceptT String IO WidgetSentenceBranch
createWidgetSentenceBranch tc so q = do
  req@(Request sts) <-
    withExceptT show
      . except
      $ parse requestParser "createWidgetSentenceBranch" q
  affectedFileCount <-
    lift $
      HS.size
        . combinableSentenceResultSet
        <$> queryRequest tc req
  return $ WidgetSentenceBranch q (SentenceBranch so sts) affectedFileCount
