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
import Data.Event (TaggerEvent)
import Data.Model (TaggerModel)
import Data.Text (Text)
import qualified Data.Text as T
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
  ( Show a
  , Show b
  , HasNestedOperand s1 Bool
  , HasWidget s2 (WidgetNode TaggerModel TaggerEvent)
  , HasWidget s1 (WidgetNode TaggerModel TaggerEvent)
  ) =>
  Text ->
  a ->
  b ->
  s2 ->
  s1 ->
  WidgetNode TaggerModel TaggerEvent
tewbRngWidget labelText li i x y =
  withStyleBasic [paddingT 3, paddingR 3, paddingB 3] $
    vstack
      [ x ^. widget
      , hstack
          [ consLabel
          , spacer
          , (if y ^. nestedOperand then mkParens else id) $ y ^. widget
          ]
      ]
 where
  mkParens w = withStyleBasic [border 1 black, padding 3] w
  consLabel = label_ labelText [resizeFactor (-1)]

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
    f li i x y =
      withStyleBasic [border 1 black, padding 3] $
        vstack
          [ box_ [alignTop] $ x ^. widget
          , withStyleBasic [paddingB 3] $
              vstack
                [ withStyleBasic [paddingB 5] $
                    separatorLine_ [resizeFactor (-1)]
                , separatorLine_ [resizeFactor (-1)]
                ]
          , box_ [alignTop, alignCenter] $ y ^. widget
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
            (draggable combinedExpr $ wf li i x y)
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
  f li i d' = ExpressionWidgetState w (pure d') False
   where
    dTermLabelText = case d' of
      DTerm (Pattern t) -> "d." <> t
      DMetaTerm (Pattern t) -> t
      _synonymNotMatched -> T.pack . show $ d'
    w =
      draggable (pure d' :: TagExpression (DTerm Pattern)) $
        label_ dTermLabelText [resizeFactor (-1)]

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
            ( draggable combinedExpr $
                withStyleBasic [paddingT 3, paddingR 3, paddingB 3] $
                  vstack
                    [ x ^. widget
                    , hstack
                        [ consLabel
                        , spacer
                        , (if y ^. nestedOperand then mkParens else id) $ y ^. widget
                        ]
                    ]
            )
            combinedExpr
            True
     where
      mkParens w = withStyleBasic [border 1 black, padding 3] w

      consLabel = label_ l [resizeFactor (-1)]

qewbLeaf :: QueryLeaf -> QueryExpressionWidgetBuilder
qewbLeaf ql =
  let qe = QueryExpression . pure $ ql
   in case ql of
        FileLeaf pat -> QueryExpressionWidgetBuilderG $ do
          count <- getIncr
          incr
          pure $
            ExpressionWidgetState
              ( draggable qe $
                  label_ ("p." <> patternText pat) [resizeFactor (-1)]
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
              (draggable qe $ teews ^. widget)
              qe
              False
