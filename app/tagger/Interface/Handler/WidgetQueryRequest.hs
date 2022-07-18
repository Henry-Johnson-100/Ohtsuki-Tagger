{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Interface.Handler.WidgetQueryRequest (
  WidgetQueryRequest (widgetQueryRequest),
  WidgetSentenceBranch (
    widgetSentenceBranchText,
    widgetSentenceBranch,
    widgetSentenceBranchCount
  ),
  pattern WidgetSentenceBranchComp,
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
import Data.Sequence (Seq, deleteAt, elemIndexL, empty, insertAt, (|>))
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import Database.Tagger.Type (TaggedConnection)
import Text.TaggerQL (combinableSentenceResultSet, queryRequest)
import Text.TaggerQL.AST (
  Request (Request),
  SentenceSet (..),
  SentenceTree (..),
 )
import Text.TaggerQL.Parser.Internal (parse, requestParser)

newtype WidgetQueryRequest = WidgetQueryRequest
  { widgetQueryRequest :: Seq WidgetSentenceBranch
  }
  deriving (Show, Eq, Semigroup, Monoid)

data WidgetSentenceBranch = WidgetSentenceBranch
  { widgetSentenceBranchText :: Text
  , widgetSentenceBranch :: SentenceTree Text
  , widgetSentenceBranchCount :: Int
  }
  deriving (Show, Eq)

pattern WidgetSentenceBranchComp :: Text -> Int -> SetOp -> WidgetSentenceBranch
pattern WidgetSentenceBranchComp t c so <-
  WidgetSentenceBranch t (sentenceTreeSetOp -> so) c

widgetSentenceBranchSetOp :: WidgetSentenceBranch -> SetOp
widgetSentenceBranchSetOp (widgetSentenceBranch -> st) = sentenceTreeSetOp st

sentenceTreeSetOp :: SentenceTree a -> SetOp
sentenceTreeSetOp st =
  case st of
    SentenceBranch so _ -> so
    SentenceNode (SentenceSet so _) -> so

emptyWidgetQueryRequest :: WidgetQueryRequest
emptyWidgetQueryRequest = WidgetQueryRequest empty

squashWidgetQueryRequest :: WidgetQueryRequest -> Request Text
squashWidgetQueryRequest (WidgetQueryRequest sts) =
  Request . toList $
    (widgetSentenceBranch <$> sts)

moveQueryWidgetNodeTo ::
  WidgetSentenceBranch ->
  WidgetSentenceBranch ->
  WidgetQueryRequest ->
  WidgetQueryRequest
moveQueryWidgetNodeTo from to wr@(WidgetQueryRequest sts) = fromMaybe wr $ do
  deleteIx <- elemIndexL from sts
  let removedFromNode = deleteAt deleteIx sts
  toIx <- elemIndexL to removedFromNode
  return . WidgetQueryRequest $ insertAt toIx from removedFromNode

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
