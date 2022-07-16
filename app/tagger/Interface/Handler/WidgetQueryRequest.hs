{-# LANGUAGE NoImplicitPrelude #-}

module Interface.Handler.WidgetQueryRequest (
  WidgetQueryRequest (widgetQueryRequest),
  WidgetSentenceBranch (
    widgetSentenceBranchText,
    widgetSentenceBranch,
    widgetSentenceBranchCount
  ),
  emptyWidgetQueryRequest,
  squashWidgetQueryRequest,
  moveQueryWidgetNodeTo,
  deleteWidgetQueryNode,
  appendWidgetQueryNode,
  createWidgetSentenceBranch,
) where

import Control.Monad ((<=<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Foldable (toList)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, deleteAt, elemIndexL, empty, insertAt, lookup, (|>))
import Data.Tagger (SetOp)
import Data.Text (Text)
import Database.Tagger.Type (TaggedConnection)
import System.IO (hPrint, stderr)
import Tagger.Util (hoistMaybe)
import Text.TaggerQL (combinableSentenceResultSet, queryRequest)
import Text.TaggerQL.AST (
  Request (Request),
  SentenceTree (SentenceBranch),
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
  MaybeT IO WidgetSentenceBranch
createWidgetSentenceBranch tc so q = do
  let parseResult = parse requestParser "" q
  req@(Request sts) <-
    either
      (const (hoistMaybe Nothing) <=< lift . hPrint stderr)
      return
      parseResult
  affectedFileCount <-
    lift $
      HS.size
        . combinableSentenceResultSet
        <$> queryRequest tc req
  return $ WidgetSentenceBranch q (SentenceBranch so sts) affectedFileCount
