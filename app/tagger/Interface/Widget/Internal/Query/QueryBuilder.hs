{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.Query.QueryBuilder (
  queryBuilderWidget,
) where

import Control.Lens
import Control.Monad.Trans.State.Strict
import Data.Event
import Data.Model
import Data.Tagger
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Engine

queryBuilderWidget :: TaggerModel -> TaggerWidget
queryBuilderWidget m =
  fst
    . flip evalState 1
    . runExpressionInterpreter queryBuilder
    $ m ^. fileSelectionModel . queryModel . expression
 where
  queryBuilder ::
    ExpressionInterpreter
      (State Int)
      (TaggerWidget, SubExpression)
      (TaggerWidget, Expression)
  queryBuilder =
    ExpressionInterpreter
      { subExpressionInterpreter =
          SubExpressionInterpreter
            { interpretSubTag = subTagWidget
            , interpretBinarySubExpression = binarySubExpressionWidget
            , interpretSubExpression = subExpressionWidget
            }
      , interpretFileTerm = fileTermExpressionWidget
      , interpretTagTerm = tagTermValueWidget
      , interpretBinaryExpression = binaryExpressionWidget
      , interpretTagExpression = tagExpressionWidget
      }

subExpressionWidget ::
  TagTermExtension (State Int (TaggerWidget, SubExpression)) ->
  State Int (TaggerWidget, SubExpression)
subExpressionWidget (TagTermExtension tt se') = do
  (seW, se) <- se'
  n <- get
  let ex = SubExpression $ TagTermExtension tt se
      uib _wenv _model =
        draggable tt $
          label_
            ( case tt of
                DescriptorTerm txt -> "d." <> txt
                MetaDescriptorTerm txt -> txt
            )
            [resizeFactor (-1)] ::
          WidgetNode QueryBuilderModel QueryBuilderEvent
      eh _wenv _node _model event =
        case event of
          _nothingHere -> []
      c =
        draggable ex $
          vstack
            [ composite "SubExpressionWidget" (plens n) uib eh
            , hstack
                [ label_ "{" [resizeFactor (-1)]
                , seW
                , label_ "}" [resizeFactor (-1)]
                ]
            ]
  modify (1 +)
  return (c, ex)

binarySubExpressionWidget ::
  BinaryOperation (TaggerWidget, SubExpression) ->
  State Int (TaggerWidget, SubExpression)
binarySubExpressionWidget bn@(BinaryOperation (lhsW, _) so (rhsW, rhs)) = do
  n <- get
  let ex = BinarySubExpression $ fmap snd bn
      rhsIsNested = case rhs of
        BinarySubExpression _ -> True
        _notNested -> False
      uib _wenv _model =
        label_
          ( case so of
              Union -> "|"
              Intersect -> "&"
              Difference -> "!"
          )
          [resizeFactor (-1)] ::
          WidgetNode QueryBuilderModel QueryBuilderEvent
      eh _wenv _node _model event =
        case event of
          _nothingHere -> []
      c =
        draggable ex $
          hstack
            [ lhsW
            , composite "BinarySubExpressionWidget" (plens n) uib eh
            , if rhsIsNested
                then
                  hstack
                    [ label_ "(" [resizeFactor (-1)]
                    , rhsW
                    , label_ ")" [resizeFactor (-1)]
                    ]
                else rhsW
            ]
  modify (1 +)
  return (c, ex)

subTagWidget :: TagTerm -> State Int (TaggerWidget, SubExpression)
subTagWidget tt = do
  n <- get
  let ex = SubTag tt
      uib _wenv _model =
        draggable tt $
          label_
            ( case tt of
                DescriptorTerm txt -> "d." <> txt
                MetaDescriptorTerm txt -> txt
            )
            [resizeFactor (-1)] ::
          WidgetNode QueryBuilderModel QueryBuilderEvent
      eh _wenv _node _model event =
        case event of
          _nothingHere -> []
      c = composite "SubTagWidget" (plens n) uib eh
  modify (1 +)
  return (c, ex)

tagExpressionWidget ::
  TagTermExtension (State Int (TaggerWidget, SubExpression)) ->
  State Int (TaggerWidget, Expression)
tagExpressionWidget (TagTermExtension tt sem) = do
  (seW, se) <- sem
  n <- get
  let ex = TagExpression $ TagTermExtension tt se
      uib _wenv _model =
        draggable tt $
          label_
            ( case tt of
                DescriptorTerm txt -> "d." <> txt
                MetaDescriptorTerm txt -> txt
            )
            [resizeFactor (-1)] ::
          WidgetNode QueryBuilderModel QueryBuilderEvent
      eh _wenv _node _model event =
        case event of
          _nothingHere -> []
      c =
        draggable ex $
          vstack
            [ composite "TagExpressionWidget" (plens n) uib eh
            , hstack
                [ label_ "{" [resizeFactor (-1)]
                , seW
                , label_ "}" [resizeFactor (-1)]
                ]
            ]
  modify (1 +)
  return (c, ex)

binaryExpressionWidget ::
  BinaryOperation (TaggerWidget, Expression) ->
  State Int (TaggerWidget, Expression)
binaryExpressionWidget (BinaryOperation (lhsW, lhs) so (rhsW, rhs)) = do
  n <- get
  let ex = BinaryExpression $ BinaryOperation lhs so rhs
      rhsIsNested = case rhs of
        BinaryExpression _ -> True
        _notNested -> False
      uib _wenv _model =
        dropdownV
          so
          (\_ so' -> UpdateBinarySetOp so')
          [Union, Intersect, Difference]
          ( \so' ->
              label_
                (case so' of Union -> "|"; Intersect -> "&"; Difference -> "!")
                [resizeFactor (-1)]
          )
          ( \so' ->
              label_
                (case so' of Union -> "|"; Intersect -> "&"; Difference -> "!")
                [resizeFactor (-1)]
          ) ::
          WidgetNode QueryBuilderModel QueryBuilderEvent
      eh _wenv _node model event =
        case event of
          UpdateBinarySetOp so' ->
            [ Model $
                model & _1
                  .~ ( case model ^. _1 of
                        Just (Right (BinaryExpression (BinaryOperation lhs' _ rhs'))) ->
                          Just . Right . BinaryExpression $ BinaryOperation lhs' so' rhs'
                        other -> other
                     )
            ] ::
              [ EventResponse
                  QueryBuilderModel
                  QueryBuilderEvent
                  TaggerModel
                  TaggerEvent
              ]
      c =
        draggable ex $
          hstack
            [ lhsW
            , composite "BinaryExpressionWidget" (plens n) uib eh
            , if rhsIsNested
                then
                  hstack
                    [ label_ "(" [resizeFactor (-1)]
                    , rhsW
                    , label_ ")" [resizeFactor (-1)]
                    ]
                else rhsW
            ]
  modify (1 +)
  return (c, ex)

tagTermValueWidget :: TagTerm -> State Int (TaggerWidget, Expression)
tagTermValueWidget tt = do
  n <- get
  let ex = TagTermValue tt
      uib _wenv _model =
        draggable tt $
          label_
            ( case tt of
                DescriptorTerm txt -> "d." <> txt
                MetaDescriptorTerm txt -> txt
            )
            [resizeFactor (-1)] ::
          WidgetNode QueryBuilderModel QueryBuilderEvent
      eh _wenv _node _model event =
        case event of
          _nothingHere -> []
      c = composite "TagTermValueWidget" (plens n) uib eh
  modify (1 +)
  return (c, ex)

fileTermExpressionWidget :: FileTerm -> State Int (TaggerWidget, Expression)
fileTermExpressionWidget ft@(FileTerm txt) = do
  n <- get
  let ex = FileTermValue ft
      uib _wenv _model =
        draggable ft $
          label_ ("p." <> txt) [resizeFactor (-1)] ::
          WidgetNode QueryBuilderModel QueryBuilderEvent
      eh _wenv _node _model event = case event of
        _nothingHere -> []
      c = composite "FileTermExpressionWidget" (plens n) uib eh
  modify (1 +)
  return (c, ex)

plens :: Int -> Lens' TaggerModel QueryBuilderModel
plens n =
  lens
    ( \tm ->
        (tm ^. fileSelectionModel . queryModel . expression . expressionIx n, True)
    )
    ( \tm (meexpr, _) ->
        tm
          & fileSelectionModel . queryModel . expression . expressionIx n .~ meexpr
    )

type QueryBuilderModel = (Maybe (Either SubExpression Expression), Bool)

data QueryBuilderEvent
  = UpdateBinarySetOp SetOp
