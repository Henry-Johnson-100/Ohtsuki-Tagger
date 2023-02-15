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

import Control.Lens (
  Identity (runIdentity),
  abbreviatedFields,
  makeLensesWith,
  (&),
  (.~),
  (<&>),
  (^.),
 )
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Coerce (coerce)
import Data.Data (Typeable)
import Data.Event (QueryEvent (CycleRingOperator, DeleteRingOperand, LeftDistribute, PlaceTagExpression), TaggerEvent (..))
import Data.Model (HasQueryEditMode (queryEditMode), Latitude (..), TaggerModel)
import Data.Text (Text)
import qualified Data.Text as T
import Interface.Theme (yuiBlue, yuiOrange, yuiPeach, yuiRed)
import Interface.Widget.Internal.Core
import Monomer
import Monomer.Lens (a)
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

expressionWidget :: FreeQueryExpression -> TaggerWidget
expressionWidget =
  box_
    [ alignCenter
    , -- Only trigger a merge if this widget is actually visible
      mergeRequired (\_wenv _x y -> y ^. queryEditMode)
    ]
    . (^. widget)
    . fst
    . runIdentity
    . runCounter
    . coerce
    . evaluateRingExpression
    . buildExpressionWidget

buildExpressionWidget ::
  FreeQueryExpression ->
  RingExpression QueryExpressionWidgetBuilder
buildExpressionWidget (FreeQueryExpression fqe) = do
  fqe' <- fqe
  either
    ( \(x, y) ->
        let x' =
              (\(QueryExpressionWidgetBuilderG z) -> z)
                . evaluateRingExpression
                . buildExpressionWidget
                $ x
            y' = (\(TagExpressionWidgetBuilderG z) -> z) . buildTagExpressionWidget $ y
            combined = do
              (ExpressionWidgetState xx xy _) <- x'
              (ExpressionWidgetState yx yy _) <- fst <$> runCounter y'
              _count <- getIncr
              incr
              pure $
                ExpressionWidgetState
                  (withStyleBasic [border 1 yuiRed] $ vstack [xx, yx])
                  (xy <-# yy)
                  True
         in pure . QueryExpressionWidgetBuilderG $ combined
    )
    (pure . qewbLeaf)
    fqe'

{- |
 A generic form of 'TagExpressionWidgetBuilder` for deriving newtype instances.
-}
newtype TagExpressionWidgetBuilderG m a
  = TagExpressionWidgetBuilderG
      (CounterT m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

type TagExpressionWidgetBuilder =
  TagExpressionWidgetBuilderG
    (CounterT Identity)
    (ExpressionWidgetState TagQuery)

instance
  Rng
    ( TagExpressionWidgetBuilderG
        (CounterT Identity)
        (ExpressionWidgetState TagQuery)
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
          , withStyleBasic [textColor $ black & a .~ defaultElementOpacity / 2] $
              styledButton_
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
              , withStyleBasic [textColor $ black & a .~ defaultElementOpacity / 2] $
                  styledButton_
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
        (ExpressionWidgetState TagQuery)
    )
  where
  (∙) = tewbBinHelper (∙) f
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
  f li i d' = ExpressionWidgetState w (pure d') False
   where
    dTermLabelText = case d' of
      DTerm (Pattern t) -> "d." <> t
      DMetaTerm (Pattern t) -> t
      _synonymNotMatched -> T.pack . show $ d'
    w =
      withStyleHover [bgColor $ yuiBlue & a .~ defaultElementOpacity]
        . draggable d
        $ zstack_
          [onlyTopActive_ False]
          [ label dTermLabelText
          , let dTermDT =
                  placeTeDropTargetG
                    li
                    i
                    (pure :: DTerm Pattern -> TagQuery)
                    [border 1 yuiBlue]
                teDT =
                  placeTeDropTargetG
                    li
                    i
                    id
                    [border 1 yuiOrange]
                teDistDT = leftDistributeDropTarget li (Just i) id [border 1 yuiOrange]
                dTermDistDT =
                  leftDistributeDropTarget
                    li
                    (Just i)
                    (pure :: DTerm Pattern -> TagQuery)
                    [border 1 yuiBlue]
             in dropTargetHGrid (dTermDT .<< teDT) (teDistDT . dTermDistDT)
          ]

placeTeDropTargetG ::
  (Eq a, Typeable a) =>
  Int ->
  Int ->
  (a -> b) ->
  [StyleState] ->
  (b -> Latitude TagQuery) ->
  WidgetNode s TaggerEvent ->
  WidgetNode s TaggerEvent
placeTeDropTargetG leafIndex teIndex toTe sts c =
  dropTarget_
    (DoQueryEvent . PlaceTagExpression leafIndex teIndex . c . toTe)
    [dropTargetStyle sts]

leftDistributeDropTarget ::
  (Eq a, Typeable a) =>
  Int ->
  Maybe Int ->
  (a -> TagQuery) ->
  [StyleState] ->
  WidgetNode s TaggerEvent ->
  WidgetNode s TaggerEvent
leftDistributeDropTarget leafIndex mTeIndex toTe sts =
  dropTarget_
    (DoQueryEvent . LeftDistribute leafIndex mTeIndex . toTe)
    [dropTargetStyle sts]

{- |
 Construct an hgrid with 3 zones where each zone corresponds to a 'Latitude`
-}
dropTargetHGrid ::
  ((a -> Latitude a) -> WidgetNode s1 e1 -> WidgetNode s2 e2) ->
  (WidgetNode s3 e3 -> WidgetNode s2 e2) ->
  WidgetNode s2 e2
dropTargetHGrid dts distdts =
  vgrid
    [ hgrid ((`dts` spacer) <$> [LatLeft, LatMiddle, LatRight])
    , hgrid [distdts spacer]
    ]

infixr 9 .<<

{- |
 Compose functions that can share a common argument
-}
(.<<) :: (t -> b -> c) -> (t -> a -> b) -> t -> a -> c
(.<<) = cmpWith
 where
  cmpWith g f x = g x . f x

newtype QueryExpressionWidgetBuilderG m a
  = QueryExpressionWidgetBuilderG
      (CounterT m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

type QueryExpressionWidgetBuilder =
  QueryExpressionWidgetBuilderG Identity (ExpressionWidgetState FreeQueryExpression)

instance
  Rng
    ( QueryExpressionWidgetBuilderG
        Identity
        (ExpressionWidgetState FreeQueryExpression)
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

qewbLeaf :: Either Pattern TagQuery -> QueryExpressionWidgetBuilder
qewbLeaf ql =
  let qe = FreeQueryExpression . pure . pure $ ql
   in case ql of
        Left pat -> QueryExpressionWidgetBuilderG $ do
          _count <- getIncr
          incr
          pure $
            ExpressionWidgetState
              ( label_ ("p." <> patternText pat) [resizeFactor (-1)]
              )
              qe
              False
        Right te -> QueryExpressionWidgetBuilderG $ do
          teews <-
            fmap fst
              . runCounter
              . (\(TagExpressionWidgetBuilderG x) -> x)
              . buildTagExpressionWidget
              $ te ::
              CounterT Identity (ExpressionWidgetState TagQuery)
          incr
          pure $
            ExpressionWidgetState
              ( withStyleBasic [border 1 yuiBlue]
                  . withStyleHover [bgColor $ yuiPeach & a .~ defaultElementOpacity]
                  . draggable (teews ^. accumExpression)
                  $ teews ^. widget
              )
              qe
              False

buildTagExpressionWidget ::
  FreeCompoundExpression RingExpression MagmaExpression (DTerm Pattern) ->
  TagExpressionWidgetBuilder
buildTagExpressionWidget =
  evaluateFreeCompoundExpression evaluateRingExpression evaluateMagmaExpression
    . fmap tewbLeaf