{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Handler.WidgetQueryRequest (
  WidgetQueryRequest (widgetQueryRequest),
  WidgetQueryNode (
    widgetQueryNodeText,
    widgetQueryNode,
    widgetQueryNodeNull
  ),
  pattern WidgetQueryNodeComp,
  widgetQueryNodeTextLens,
  widgetQueryNodeLens,
  widgetQueryNodeNullLens,
  widgetQueryNodeSetOpLens,
  widgetQueryNodeIdLens,
  emptyWidgetQueryRequest,
  squashWidgetQueryRequest,
  moveQueryWidgetNodeTo,
  deleteWidgetQueryNode,
  appendWidgetQueryNode,
  createWidgetQueryNode,
  formatSentenceTree,
) where

import Control.Lens (Lens', lens, (&), (.~), (^.))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, withExceptT)
import Data.Foldable (toList)
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, deleteAt, elemIndexL, empty, insertAt, (|>))
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

data WidgetQueryNode = WidgetQueryNode
  { widgetQueryNodeText :: Text
  , widgetQueryNode :: SentenceTree Text
  , widgetQueryNodeNull :: Bool
  , -- , widgetQueryNodeCount :: Int
    widgetQueryNodeId :: Int
  }
  deriving (Show, Eq)

widgetQueryNodeTextLens :: Lens' WidgetQueryNode Text
widgetQueryNodeTextLens =
  lens
    (\(WidgetQueryNode t _ _ _) -> t)
    (\wsb t -> wsb{widgetQueryNodeText = t})

widgetQueryNodeLens :: Lens' WidgetQueryNode (SentenceTree Text)
widgetQueryNodeLens =
  lens
    (\(WidgetQueryNode _ st _ _) -> st)
    (\wsb st -> wsb{widgetQueryNode = st})

-- widgetQueryNodeCountLens :: Lens' WidgetQueryNode Int
-- widgetQueryNodeCountLens =
--   lens
--     (\(WidgetQueryNode _ _ c _) -> c)
--     (\wsb c -> wsb{widgetQueryNodeCount = c})

widgetQueryNodeNullLens :: Lens' WidgetQueryNode Bool
widgetQueryNodeNullLens =
  lens
    (\(WidgetQueryNode _ _ n _) -> n)
    (\wqn b -> wqn{widgetQueryNodeNull = b})

widgetQueryNodeIdLens :: Lens' WidgetQueryNode Int
widgetQueryNodeIdLens =
  lens
    (\(WidgetQueryNode _ _ _ k) -> k)
    (\wsb k -> wsb{widgetQueryNodeId = k})

pattern WidgetQueryNodeComp :: Text -> SentenceTree Text -> Bool -> Int -> WidgetQueryNode
pattern WidgetQueryNodeComp t ts n i <-
  WidgetQueryNode t ts n i

widgetQueryNodeSetOpLens :: Lens' WidgetQueryNode SetOp
widgetQueryNodeSetOpLens =
  lens
    (flip (^.) sentenceTreeSetOpLens . widgetQueryNode)
    (\wsb so -> wsb & widgetQueryNodeLens . sentenceTreeSetOpLens .~ so)

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
    (widgetQueryNode <$> sts)

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
      defaultId = 0
  !searchIsNull <-
    lift $
      HS.null
        . combinableSentenceResultSet
        <$> queryRequest tc req
  return $ WidgetQueryNode q (SentenceBranch explicitSetOp sts) searchIsNull defaultId

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