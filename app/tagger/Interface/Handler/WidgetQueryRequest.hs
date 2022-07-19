{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Interface.Handler.WidgetQueryRequest (
  WidgetQueryRequest (widgetQueryRequest),
  WidgetSentenceBranch (
    widgetSentenceBranchText,
    widgetSentenceBranch,
    widgetSentenceBranchCount
  ),
  pattern WidgetSentenceBranchComp,
  widgetSentenceBranchTextLens,
  widgetSentenceBranchLens,
  widgetSentenceBranchCountLens,
  widgetSentenceBranchSetOpLens,
  emptyWidgetQueryRequest,
  squashWidgetQueryRequest,
  moveQueryWidgetNodeTo,
  deleteWidgetQueryNode,
  appendWidgetQueryNode,
  createWidgetSentenceBranch,
) where

import Control.Lens (Lens', lens, (&), (.~), (^.))
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

widgetSentenceBranchTextLens :: Lens' WidgetSentenceBranch Text
widgetSentenceBranchTextLens =
  lens
    (\(WidgetSentenceBranch t _ _) -> t)
    (\wsb t -> wsb{widgetSentenceBranchText = t})

widgetSentenceBranchLens :: Lens' WidgetSentenceBranch (SentenceTree Text)
widgetSentenceBranchLens =
  lens
    (\(WidgetSentenceBranch _ st _) -> st)
    (\wsb st -> wsb{widgetSentenceBranch = st})

widgetSentenceBranchCountLens :: Lens' WidgetSentenceBranch Int
widgetSentenceBranchCountLens =
  lens
    (\(WidgetSentenceBranch _ _ c) -> c)
    (\wsb c -> wsb{widgetSentenceBranchCount = c})

pattern WidgetSentenceBranchComp :: Text -> Int -> SetOp -> WidgetSentenceBranch
pattern WidgetSentenceBranchComp t c so <-
  WidgetSentenceBranch t ((^. sentenceTreeSetOpLens) -> so) c

widgetSentenceBranchSetOpLens :: Lens' WidgetSentenceBranch SetOp
widgetSentenceBranchSetOpLens =
  lens
    (flip (^.) sentenceTreeSetOpLens . widgetSentenceBranch)
    (\wsb so -> wsb & widgetSentenceBranchLens . sentenceTreeSetOpLens .~ so)

sentenceTreeSetOpLens :: Lens' (SentenceTree a) SetOp
sentenceTreeSetOpLens =
  lens
    ( \st -> case st of
        SentenceBranch so _ -> so
        SentenceNode (SentenceSet so _) -> so
    )
    ( \st so ->
        case st of
          SentenceBranch _ xs -> SentenceBranch so xs
          SentenceNode (SentenceSet _ xs) -> SentenceNode (SentenceSet so xs)
    )

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
  Text ->
  ExceptT String IO WidgetSentenceBranch
createWidgetSentenceBranch tc q = do
  req@(Request sts) <-
    withExceptT show
      . except
      $ parse requestParser "createWidgetSentenceBranch" q
  let explicitSetOp =
        case sts of
          [] -> Union
          x : _ -> x ^. sentenceTreeSetOpLens
  affectedFileCount <-
    lift $
      HS.size
        . combinableSentenceResultSet
        <$> queryRequest tc req
  return $ WidgetSentenceBranch q (SentenceBranch explicitSetOp sts) affectedFileCount
