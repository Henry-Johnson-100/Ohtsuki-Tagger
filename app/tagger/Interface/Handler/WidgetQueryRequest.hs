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
  widgetSentenceBranchIdLens,
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
  { widgetQueryRequest :: Seq WidgetSentenceBranch
  }
  deriving (Show, Eq, Semigroup, Monoid)

data WidgetSentenceBranch = WidgetSentenceBranch
  { widgetSentenceBranchText :: Text
  , widgetSentenceBranch :: SentenceTree Text
  , widgetSentenceBranchCount :: Int
  , widgetSentenceBranchId :: Int
  }
  deriving (Show, Eq)

widgetSentenceBranchTextLens :: Lens' WidgetSentenceBranch Text
widgetSentenceBranchTextLens =
  lens
    (\(WidgetSentenceBranch t _ _ _) -> t)
    (\wsb t -> wsb{widgetSentenceBranchText = t})

widgetSentenceBranchLens :: Lens' WidgetSentenceBranch (SentenceTree Text)
widgetSentenceBranchLens =
  lens
    (\(WidgetSentenceBranch _ st _ _) -> st)
    (\wsb st -> wsb{widgetSentenceBranch = st})

widgetSentenceBranchCountLens :: Lens' WidgetSentenceBranch Int
widgetSentenceBranchCountLens =
  lens
    (\(WidgetSentenceBranch _ _ c _) -> c)
    (\wsb c -> wsb{widgetSentenceBranchCount = c})

widgetSentenceBranchIdLens :: Lens' WidgetSentenceBranch Int
widgetSentenceBranchIdLens =
  lens
    (\(WidgetSentenceBranch _ _ _ k) -> k)
    (\wsb k -> wsb{widgetSentenceBranchId = k})

pattern WidgetSentenceBranchComp :: Text -> Int -> SetOp -> WidgetSentenceBranch
pattern WidgetSentenceBranchComp t c so <-
  WidgetSentenceBranch t ((^. sentenceTreeSetOpLens) -> so) c _

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

deleteWidgetQueryNode :: WidgetSentenceBranch -> WidgetQueryRequest -> WidgetQueryRequest
deleteWidgetQueryNode wsb wr@(WidgetQueryRequest sts) =
  maybe
    wr
    (WidgetQueryRequest . flip deleteAt sts)
    (elemIndexL wsb sts)

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
  return $ WidgetSentenceBranch q (SentenceBranch explicitSetOp sts) affectedFileCount 0

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