{-# LANGUAGE PatternSynonyms #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.AST.Lens (
  sentenceTreeSetOpL,
  sentenceSetSentenceL,
  sentenceSetSetOpL,
  sentenceL,
  termTreeNodeL,
  complexTermNodeL,
  pattern ComplexTermList,
  complexTermListL,
  simpleTermL,
  termCriteriaL,
  termPatternL,
) where

import Control.Lens (Lens', lens, (&), (.~), (^.))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Tagger
import Text.TaggerQL.AST

sentenceTreeSetOpL :: Lens' (SentenceTree a) SetOp
sentenceTreeSetOpL =
  lens
    ( \st ->
        case st of
          SentenceNode ss -> ss ^. sentenceSetSetOpL
          SentenceBranch so _ -> so
    )
    ( \st so ->
        case st of
          SentenceNode ss -> SentenceNode $ ss & sentenceSetSetOpL .~ so
          SentenceBranch _ sts -> SentenceBranch so sts
    )

{- |
 A lens defning the getter and setter on the 'Sentence` of a 'SentenceSet`
-}
sentenceSetSentenceL :: Lens' (SentenceSet a) (Sentence a)
sentenceSetSentenceL =
  lens
    (\(SentenceSet _ s) -> s)
    (\(SentenceSet so _) s -> SentenceSet so s)

{- |
 A lens defining a getter and setter on the 'SetOp` of a 'SentenceSet`
-}
sentenceSetSetOpL :: Lens' (SentenceSet a) SetOp
sentenceSetSetOpL =
  lens
    (\(SentenceSet so _) -> so)
    (\(SentenceSet _ s) so -> SentenceSet so s)

{- |
 A lens defining a constructor and unwrapper for the newtype 'Sentence`
-}
sentenceL :: Lens' (Sentence a) [TermTree a]
sentenceL =
  lens
    sentence
    (\_ tts -> Sentence tts)

{- |
 lens defining the getter and setter for the head node 'Term` in a 'TermTree`

 Preserves 'Simple` and 'Complex` distinction when set.
-}
termTreeNodeL :: Lens' (TermTree a) (Term a)
termTreeNodeL =
  lens
    ( \tt -> case tt of
        Simple t -> t ^. simpleTermL
        Complex ct -> ct ^. complexTermNodeL
    )
    ( \tt t -> case tt of
        Simple _discard -> Simple . SimpleTerm $ t
        Complex ct -> Complex $ ct & complexTermNodeL .~ t
    )

{- |
 Lens defining a getter and setter for the head node 'Term` of a 'ComplexTerm`
-}
complexTermNodeL :: Lens' (ComplexTerm a) (Term a)
complexTermNodeL =
  lens
    complexTermNode
    ( \ct t ->
        case ct of
          _discard :<- ncts -> t :<- ncts
          Bottom _discard -> Bottom t
    )

{- |
 A bidirectional pattern synonym which exposes the head term and
  the 'NonEmpty` component of a
 'ComplexTerm` as a list. If the 'ComplexTerm` is 'Bottom` then it returns
 an empty list.

 When constructing, any list that is non-empty will create a 'ComplexTerm` using ':<-`
 otherwise, a 'Bottom` is constructed.
-}
pattern ComplexTermList :: Term a -> [ComplexTerm a] -> ComplexTerm a
pattern ComplexTermList t cts <-
  ( \ct ->
      case ct of
        t :<- (nct :| ncts) -> (t, nct : ncts)
        Bottom t -> (t, []) ->
      (t, cts)
    )
  where
    ComplexTermList t cts =
      case cts of
        [] -> Bottom t
        _nonEmpty -> t :<- NE.fromList cts

{- |
 Derived lens defining a getter and setter on the list field of the
 bidirectional 'ComplexTermList` pattern synomym

 Calling the setter reconstructs the pattern synonym which makes it possible
 to easily convert a ':<-` to a 'Bottom` by setting an empty list. 'Bottom` can likewise
 be lifted to a ':<-`.
-}
complexTermListL :: Lens' (ComplexTerm a) [ComplexTerm a]
complexTermListL =
  lens
    (\(ComplexTermList _ ncts) -> ncts)
    (\(ComplexTermList t _) ncts -> ComplexTermList t ncts)

{- |
 Lens defining a constructor for the 'SimpleTerm` newtype

 Utilized as a getter and setter for a 'Term` from a 'SimpleTerm`
-}
simpleTermL :: Lens' (SimpleTerm a) (Term a)
simpleTermL =
  lens
    (\(SimpleTerm t) -> t)
    (\_ t -> SimpleTerm t)

{- |
 Lens defining the getter and setter for the 'termCriteria` field of a 'Term`
-}
termCriteriaL :: Lens' (Term a) QueryCriteria
termCriteriaL =
  lens
    termCriteria
    (\t qc -> t{termCriteria = qc})

{- |
 Lens defining the getter and setter for the 'termPattern` field of a 'Term`
-}
termPatternL :: Lens' (Term a) a
termPatternL =
  lens
    termPattern
    (\t tp -> t{termPattern = tp})
