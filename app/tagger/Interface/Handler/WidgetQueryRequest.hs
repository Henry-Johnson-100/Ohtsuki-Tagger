{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Handler.WidgetQueryRequest (
  WidgetQueryRequest (widgetQueryRequest),
  WidgetQueryNode (
    widgetQueryNodeText,
    widgetQueryNode,
    widgetQueryNodeCount
  ),
  pattern WidgetQueryNodeComp,
  widgetQueryNodeTextLens,
  widgetQueryNodeLens,
  widgetQueryNodeCountLens,
  widgetQueryNodeSetOpLens,
  widgetQueryNodeIdLens,
  emptyWidgetQueryRequest,
  squashWidgetQueryRequest,
  moveQueryWidgetNodeTo,
  deleteWidgetQueryNode,
  appendWidgetQueryNode,
  createWidgetQueryNode,
) where

import Control.Lens (Lens', lens, (&), (.~), (^.))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, withExceptT)
import Data.Foldable (toList)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, deleteAt, elemIndexL, empty, fromList, insertAt, (|>))
import Data.Tagger (QueryCriteria (DescriptorCriteria, FilePatternCriteria, MetaDescriptorCriteria, UntaggedCriteria), SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Type (TaggedConnection)
import Text.TaggerQL (combinableSentenceResultSet, queryRequest)
import Text.TaggerQL.AST
import Text.TaggerQL.Parser.Internal (parse, requestParser)

newtype WidgetQueryRequest = WidgetQueryRequest
  { widgetQueryRequest :: Seq WidgetQueryNode
  }
  deriving (Show, Eq, Semigroup, Monoid)

data WidgetSentenceTree
  = WidgetSentenceNode (SentenceSet Text)
  | WidgetSentenceBranch SetOp (Seq WidgetSentenceTree)
  deriving (Show, Eq)

toWidgetSentenceTree :: SentenceTree Text -> WidgetSentenceTree
toWidgetSentenceTree st =
  case st of
    SentenceNode ss -> WidgetSentenceNode ss
    SentenceBranch so st' ->
      WidgetSentenceBranch so . fromList $
        (toWidgetSentenceTree <$> st')

fromWidgetSentenceTree :: WidgetSentenceTree -> SentenceTree Text
fromWidgetSentenceTree st =
  case st of
    WidgetSentenceNode ss -> SentenceNode ss
    WidgetSentenceBranch so wst ->
      SentenceBranch so . toList $
        (fromWidgetSentenceTree <$> wst)

data WidgetQueryNode = WidgetQueryNode
  { widgetQueryNodeText :: Text
  , widgetQueryNode :: WidgetSentenceTree
  , widgetQueryNodeCount :: Int
  , widgetQueryNodeId :: Int
  }
  deriving (Show, Eq)

widgetQueryNodeTextLens :: Lens' WidgetQueryNode Text
widgetQueryNodeTextLens =
  lens
    (\(WidgetQueryNode t _ _ _) -> t)
    (\wsb t -> wsb{widgetQueryNodeText = t})

widgetQueryNodeLens :: Lens' WidgetQueryNode WidgetSentenceTree
widgetQueryNodeLens =
  lens
    (\(WidgetQueryNode _ st _ _) -> st)
    (\wsb st -> wsb{widgetQueryNode = st})

widgetQueryNodeCountLens :: Lens' WidgetQueryNode Int
widgetQueryNodeCountLens =
  lens
    (\(WidgetQueryNode _ _ c _) -> c)
    (\wsb c -> wsb{widgetQueryNodeCount = c})

widgetQueryNodeIdLens :: Lens' WidgetQueryNode Int
widgetQueryNodeIdLens =
  lens
    (\(WidgetQueryNode _ _ _ k) -> k)
    (\wsb k -> wsb{widgetQueryNodeId = k})

pattern WidgetQueryNodeComp :: Text -> Int -> SetOp -> WidgetQueryNode
pattern WidgetQueryNodeComp t c so <-
  WidgetQueryNode t ((^. widgetSentenceTreeSetOpLens) -> so) c _

widgetQueryNodeSetOpLens :: Lens' WidgetQueryNode SetOp
widgetQueryNodeSetOpLens =
  lens
    (flip (^.) widgetSentenceTreeSetOpLens . widgetQueryNode)
    (\wsb so -> wsb & widgetQueryNodeLens . widgetSentenceTreeSetOpLens .~ so)

widgetSentenceTreeSetOpLens :: Lens' WidgetSentenceTree SetOp
widgetSentenceTreeSetOpLens =
  lens
    ( \st -> case st of
        WidgetSentenceNode (SentenceSet so _) -> so
        WidgetSentenceBranch so _ -> so
    )
    ( \st so ->
        case st of
          WidgetSentenceNode (SentenceSet _ ss) -> WidgetSentenceNode (SentenceSet so ss)
          WidgetSentenceBranch _ wst -> WidgetSentenceBranch so wst
    )

sentenceTreeSetOpLens :: Lens' (SentenceTree a) SetOp
sentenceTreeSetOpLens =
  lens
    ( \st -> case st of
        SentenceNode (SentenceSet so _) -> so
        SentenceBranch so _ -> so
    )
    ( \st so ->
        case st of
          SentenceNode (SentenceSet _ ss) -> SentenceNode (SentenceSet so ss)
          SentenceBranch _ st' -> SentenceBranch so st'
    )

emptyWidgetQueryRequest :: WidgetQueryRequest
emptyWidgetQueryRequest = WidgetQueryRequest empty

squashWidgetQueryRequest :: WidgetQueryRequest -> Request Text
squashWidgetQueryRequest (WidgetQueryRequest sts) =
  Request . toList $
    (fromWidgetSentenceTree . widgetQueryNode <$> sts)

moveQueryWidgetNodeTo ::
  WidgetQueryNode ->
  WidgetQueryNode ->
  WidgetQueryRequest ->
  WidgetQueryRequest
moveQueryWidgetNodeTo from to wr@(WidgetQueryRequest sts) = fromMaybe wr $ do
  deleteIx <- elemIndexL from sts
  let removedFromNode = deleteAt deleteIx sts
  toIx <- elemIndexL to removedFromNode
  return . WidgetQueryRequest $ insertAt toIx from removedFromNode

deleteWidgetQueryNode :: WidgetQueryNode -> WidgetQueryRequest -> WidgetQueryRequest
deleteWidgetQueryNode wsb wr@(WidgetQueryRequest sts) =
  maybe
    wr
    (WidgetQueryRequest . flip deleteAt sts)
    (elemIndexL wsb sts)

appendWidgetQueryNode ::
  WidgetQueryNode ->
  WidgetQueryRequest ->
  WidgetQueryRequest
appendWidgetQueryNode wsb (WidgetQueryRequest sts) =
  WidgetQueryRequest $ sts |> wsb

createWidgetQueryNode ::
  TaggedConnection ->
  Text ->
  ExceptT String IO WidgetQueryNode
createWidgetQueryNode tc q = do
  req@(Request sts) <-
    withExceptT show
      . except
      $ parse requestParser "createWidgetQueryNode" q
  let explicitSetOp =
        case sts of
          [] -> Intersect
          x : _ -> x ^. sentenceTreeSetOpLens
  affectedFileCount <-
    lift $
      HS.size
        . combinableSentenceResultSet
        <$> queryRequest tc req
  return $
    WidgetQueryNode
      q
      ( WidgetSentenceBranch
          explicitSetOp
          (fromList (toWidgetSentenceTree <$> sts))
      )
      affectedFileCount
      0

formatSentenceTree :: SentenceTree Text -> Text
formatSentenceTree (SentenceNode ss) = formatSentenceSet ss
formatSentenceTree (SentenceBranch so sss) =
  formatSetOp so
    <> "( "
    <> T.unwords (formatSentenceTree <$> sss)
    <> " )"

formatSentenceSet :: SentenceSet Text -> Text
formatSentenceSet (SentenceSet so s) =
  formatSetOp so
    <> formatSentence s

formatSentence :: Sentence Text -> Text
formatSentence (Sentence tts) = T.unwords $ formatTermTree <$> tts

formatTermTree :: TermTree Text -> Text
formatTermTree (Simple st) = formatSimpleTerm st
formatTermTree (Complex ct) = formatComplexTerm ct

formatComplexTerm :: ComplexTerm Text -> Text
formatComplexTerm (t :<- ct) =
  formatTerm t <> "{ "
    <> (T.unwords . NE.toList . NE.map formatComplexTerm $ ct)
    <> " }"
formatComplexTerm (Bottom t) = formatTerm t

formatSimpleTerm :: SimpleTerm Text -> Text
formatSimpleTerm (SimpleTerm t) = formatTerm t

formatTerm :: Term Text -> Text
formatTerm (Term qc t) = formatQueryCriteria qc <> t

formatQueryCriteria :: QueryCriteria -> Text
formatQueryCriteria DescriptorCriteria = "D."
formatQueryCriteria MetaDescriptorCriteria = "R."
formatQueryCriteria FilePatternCriteria = "P."
formatQueryCriteria UntaggedCriteria = "U."

formatSetOp :: SetOp -> Text
formatSetOp Union = "U|"
formatSetOp Intersect = "I|"
formatSetOp Difference = "D|"