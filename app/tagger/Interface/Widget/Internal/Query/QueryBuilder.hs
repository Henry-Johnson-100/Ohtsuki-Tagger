{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# HLINT ignore "Redundant ^." #-}
{-# HLINT ignore "Use let" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.Query.QueryBuilder (
  expressionWidget,
  queryEditorTextFieldKey,
) where

import Control.Lens hiding (Magma, index, (#))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Data (Typeable)
import Data.Event (QueryEvent (CycleRingOperator, DeleteRingOperand), TaggerEvent (..))
import Data.Model (TaggerModel)
import Data.Text (Text)
import qualified Data.Text as T
import Interface.Theme (yuiBlue)
import Interface.Widget.Internal.Core
import Monomer
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.AST.Editor

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

{- |
 A stateful accumulator that builds a total widget out of some given expression e.
-}
data ExpressionWidgetState e = ExpressionWidgetState
  { _expressionwidgetstateWidget :: TaggerWidget
  , _expressionwidgetstateAccumExpression :: e
  , _expressionwidgetstateNestedOperand :: Bool
  }
  deriving (Functor)

makeLensesWith abbreviatedFields ''ExpressionWidgetState

-- testquery:
-- o%yui {(gym_clothes | swimsuit | kimono){blue} hair{r%bow}! (suggestive | sweetie_cute_new_year {+})}

queryEditorTextFieldKey :: Text
queryEditorTextFieldKey = "queryEditorTextField"

expressionWidget :: QueryExpression -> TaggerWidget
expressionWidget =
  box_ [alignCenter]
    . (^. widget)
    . fst
    . runIdentity
    . runCounter
    . (\(QueryExpressionWidgetBuilderG x) -> x)
    . evaluateRing
    . fmap qewbLeaf
    . runQueryExpression

{- |
 A generic form of 'TagExpressionWidgetBuilder` for deriving newtype instances.
-}
newtype TagExpressionWidgetBuilderG m a
  = TagExpressionWidgetBuilderG
      (CounterT m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

{- |
 A builder that mimics a WithIndex counter environment so that the indices are
 exposed to the pure functions in the continuation.
-}
type TagExpressionWidgetBuilder =
  TagExpressionWidgetBuilderG
    (CounterT Identity)
    (ExpressionWidgetState (TagExpression (DTerm Pattern)))

instance
  Rng
    ( TagExpressionWidgetBuilderG
        (CounterT Identity)
        (ExpressionWidgetState (TagExpression (DTerm Pattern)))
    )
  where
  (+.) x y = tewbBinHelper (+.) (tewbRngWidget "|") x y <&> nestedOperand .~ True
  (*.) x y = tewbBinHelper (*.) (tewbRngWidget "&") x y <&> nestedOperand .~ True
  (-.) x y = tewbBinHelper (-.) (tewbRngWidget "!") x y <&> nestedOperand .~ True

tewbRngWidget ::
  ( HasNestedOperand s1 Bool
  , HasWidget s2 (WidgetNode TaggerModel TaggerEvent)
  , HasWidget s1 (WidgetNode TaggerModel TaggerEvent)
  ) =>
  Text ->
  Int ->
  Int ->
  s2 ->
  s1 ->
  WidgetNode TaggerModel TaggerEvent
tewbRngWidget labelText li i x y =
  withStyleBasic [paddingT 3, paddingR 3, paddingB 3] $
    vstack
      [ hstack
          [ x ^. widget
          , styledButton_
              []
              "X"
              (DoQueryEvent $ DeleteRingOperand li (Just i) (Left ()))
          ]
      , hstack
          [ styledButton_ [] labelText (DoQueryEvent $ CycleRingOperator li (Just i))
          , spacer
          , hstack
              [ ( if y ^. nestedOperand
                    then mkParens
                    else id
                )
                  $ y ^. widget
              , styledButton_
                  []
                  "X"
                  (DoQueryEvent $ DeleteRingOperand li (Just i) (Right ()))
              ]
          ]
      ]
 where
  mkParens w = withStyleBasic [border 1 black, padding 3] w

instance
  Magma
    ( TagExpressionWidgetBuilderG
        (CounterT Identity)
        (ExpressionWidgetState (TagExpression (DTerm Pattern)))
    )
  where
  (#) ::
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern)))
  (#) = tewbBinHelper (#) f
   where
    f _li _i x y =
      withStyleBasic [border 1 black, padding 3] $
        vstack
          [ box_ [alignTop, ignoreEmptyArea] $ x ^. widget
          , withStyleBasic [paddingB 3] $
              vstack
                [ withStyleBasic
                    [paddingB 5]
                    separatorLine
                , separatorLine
                ]
          , box_ [alignTop, alignCenter, ignoreEmptyArea] $ y ^. widget
          ]

tewbBinHelper ::
  ( Monad m
  , Eq e
  , Typeable e
  , HasAccumExpression s1 t1
  , HasAccumExpression s2 t2
  ) =>
  (t1 -> t2 -> e) ->
  (Int -> Int -> s1 -> s2 -> WidgetNode TaggerModel TaggerEvent) ->
  TagExpressionWidgetBuilderG (CounterT m) s1 ->
  TagExpressionWidgetBuilderG (CounterT m) s2 ->
  TagExpressionWidgetBuilderG
    (CounterT m)
    (ExpressionWidgetState e)
tewbBinHelper
  binComb
  wf
  (TagExpressionWidgetBuilderG lhs)
  (TagExpressionWidgetBuilderG rhs) = TagExpressionWidgetBuilderG $ do
    lhs' <- lhs
    rhs' <- rhs
    leafCount <- lift getIncr
    teCount <- getIncr
    incr
    pure $ f leafCount teCount lhs' rhs'
   where
    f li i x y =
      let combinedExpr = binComb (x ^. accumExpression) (y ^. accumExpression)
       in ExpressionWidgetState
            (wf li i x y)
            combinedExpr
            False

tewbLeaf ::
  DTerm Pattern -> TagExpressionWidgetBuilder
tewbLeaf d = TagExpressionWidgetBuilderG $ do
  leafCount <- lift getIncr
  teCount <- getIncr
  incr
  pure $ f leafCount teCount d
 where
  f _li _i d' = ExpressionWidgetState w (pure d') False
   where
    dTermLabelText = case d' of
      DTerm (Pattern t) -> "d." <> t
      DMetaTerm (Pattern t) -> t
      _synonymNotMatched -> T.pack . show $ d'
    w =
      label dTermLabelText

newtype QueryExpressionWidgetBuilderG m a
  = QueryExpressionWidgetBuilderG
      (CounterT m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

type QueryExpressionWidgetBuilder =
  QueryExpressionWidgetBuilderG Identity (ExpressionWidgetState QueryExpression)

instance
  Rng
    ( QueryExpressionWidgetBuilderG
        Identity
        (ExpressionWidgetState QueryExpression)
    )
  where
  (+.) = qewbRngHelper (+.) "|"
  (*.) = qewbRngHelper (*.) "&"
  (-.) = qewbRngHelper (-.) "!"

qewbRngHelper ::
  ( Monad m
  , HasNestedOperand s1 Bool
  , HasWidget s2 (WidgetNode TaggerModel TaggerEvent)
  , HasWidget s1 (WidgetNode TaggerModel TaggerEvent)
  , HasAccumExpression s2 t1
  , HasAccumExpression s1 t2
  , Eq e
  , Typeable e
  ) =>
  (t1 -> t2 -> e) ->
  Text ->
  QueryExpressionWidgetBuilderG m s2 ->
  QueryExpressionWidgetBuilderG m s1 ->
  QueryExpressionWidgetBuilderG m (ExpressionWidgetState e)
qewbRngHelper
  c
  l
  (QueryExpressionWidgetBuilderG lhs)
  (QueryExpressionWidgetBuilderG rhs) = QueryExpressionWidgetBuilderG $ do
    x' <- lhs
    y' <- rhs
    count <- getIncr
    incr
    pure $ f count x' y'
   where
    f i x y =
      let combinedExpr = c (x ^. accumExpression) (y ^. accumExpression)
       in ExpressionWidgetState
            ( withStyleBasic [paddingT 3, paddingR 3, paddingB 3] $
                vstack
                  [ hstack
                      [ x ^. widget
                      , styledButton_
                          []
                          "X"
                          (DoQueryEvent $ DeleteRingOperand i Nothing (Left ()))
                      ]
                  , hstack
                      [ styledButton_ [] l (DoQueryEvent $ CycleRingOperator i Nothing)
                      , spacer
                      , hstack
                          [ ( if y ^. nestedOperand
                                then mkParens
                                else id
                            )
                              $ y ^. widget
                          , styledButton_
                              []
                              "X"
                              (DoQueryEvent $ DeleteRingOperand i Nothing (Right ()))
                          ]
                      ]
                  ]
            )
            combinedExpr
            True
     where
      mkParens w = withStyleBasic [border 1 black, padding 3] w

qewbLeaf :: QueryLeaf -> QueryExpressionWidgetBuilder
qewbLeaf ql =
  let qe = QueryExpression . pure $ ql
   in case ql of
        FileLeaf pat -> QueryExpressionWidgetBuilderG $ do
          _count <- getIncr
          incr
          pure $
            ExpressionWidgetState
              ( label_ ("p." <> patternText pat) [resizeFactor (-1)]
              )
              qe
              False
        TagLeaf te -> QueryExpressionWidgetBuilderG $ do
          teews <-
            fmap fst
              . runCounter
              . (\(TagExpressionWidgetBuilderG x) -> x)
              . evaluateTagExpressionR (#)
              . fmap tewbLeaf
              $ te ::
              CounterT Identity (ExpressionWidgetState (TagExpression (DTerm Pattern)))
          incr
          pure $
            ExpressionWidgetState
              (withStyleBasic [border 1 yuiBlue] $ teews ^. widget)
              qe
              False
