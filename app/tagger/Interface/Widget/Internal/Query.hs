{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ?~" #-}

module Interface.Widget.Internal.Query (
  queryTextFieldKey,
  widget,
) where

import Control.Lens (Lens', lens, (%~), (&), (.~), (^.))
import Control.Lens.Tuple
import Control.Monad.Trans.State.Strict
import Data.Event
import Data.Model
import Data.Model.Shared.Lens (
  HasHistoryIndex (historyIndex),
  history,
  text,
 )
import Data.Tagger
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  ConcreteTag (concreteTagDescriptor),
  Descriptor (descriptor),
  File (filePath),
  query,
 )
import Interface.Theme (
  yuiBlue,
  yuiLightPeach,
  yuiOrange,
  yuiRed,
 )
import Interface.Widget.Internal.Core (
  defaultElementOpacity,
  withNodeKey,
  withStyleBasic,
 )
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  CmbAlignBottom (alignBottom),
  CmbAlignLeft (alignLeft),
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbIgnoreEmptyArea (ignoreEmptyArea),
  CmbOnChange (onChange),
  CmbPaddingL (paddingL),
  CmbPaddingT (paddingT),
  box_,
  composite,
  dropTargetStyle,
  dropTarget_,
  keystroke_,
  resizeFactor,
  textField_,
  validInput,
 )
import Monomer.Core
import Monomer.Graphics.Lens (HasA (a))
import Monomer.Widgets
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Engine
import Text.TaggerQL.Expression.Parser

widget :: TaggerModel -> TaggerWidget
widget m =
  vstack
    [ queryBuilderWidget m
    , container
        queryTextField
    ]

queryTextField :: TaggerWidget
queryTextField =
  keystroke_
    [ ("Enter", DoQueryEvent RunQueryExpression)
    , ("Up", NextHistory $ TaggerLens (fileSelectionModel . queryModel . input))
    , ("Down", PrevHistory $ TaggerLens (fileSelectionModel . queryModel . input))
    ]
    []
    . dropTarget_
      ( AppendText (TaggerLens $ fileSelectionModel . queryModel . input . text)
          . descriptor
          . concreteTagDescriptor
      )
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      ( AppendText
          ( TaggerLens $
              fileSelectionModel . queryModel . input . text
          )
          . filePath
      )
      [dropTargetStyle [border 1 yuiOrange]]
    . dropTarget_
      ( AppendText
          ( TaggerLens $
              fileSelectionModel . queryModel . input . text
          )
          . descriptor
      )
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey queryTextFieldKey
    . withStyleBasic [bgColor (yuiLightPeach & a .~ defaultElementOpacity)]
    $ textField_
      (fileSelectionModel . queryModel . input . text)
      [ onChange
          ( \t ->
              anonymousEvent
                [ Event . DoQueryEvent . OnChangeParseQueryInput $ t
                , Event $
                    if T.null . T.strip $ t
                      then
                        Mempty $
                          TaggerLens
                            ( fileSelectionModel
                                . queryModel
                                . input
                                . history
                                . historyIndex
                            )
                      else Unit ()
                ]
          )
      ]

container :: TaggerWidget -> TaggerWidget
container w =
  withStyleBasic [paddingT 5, paddingL 10]
    . box_ [alignLeft, alignBottom, ignoreEmptyArea]
    -- . withStyleBasic [borderT 1 black, borderB 1 black, borderR 1 black]
    $ w

queryTextFieldKey :: Text
queryTextFieldKey = "queryTextField"
{-# INLINE queryTextFieldKey #-}

-- Query builder widget stuff

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
            { interpretSubTag = \tt ->
                let se = SubTag tt
                    uib _wenv _model =
                      textFieldV_
                        ( case tt of
                            DescriptorTerm txt -> "d." <> txt
                            MetaDescriptorTerm txt -> txt
                        )
                        UpdateExpression
                        [validInput _2]
                    eh _wenv _node model event =
                      case event of
                        UpdateExpression t -> exprUpdateExpression model t
                        _notPossible -> []
                 in do
                      n <- get
                      let c = composite "SubTagWidget" (plens n) uib eh
                      modify (1 +)
                      return (c, se)
            , interpretBinarySubExpression =
                \bn@(BinaryOperation (lhsW, _) _ (rhsW, _)) ->
                  let ex = BinarySubExpression . fmap snd $ bn
                      uib _wenv _model = binaryOpUIB
                      eh = binaryOpEventHandler
                   in do
                        n <- get
                        let c = composite "BinarySubExpressionWidget" (plens n) uib eh
                            finalWidget = hstack [lhsW, c, rhsW]
                        modify (1 +)
                        return (finalWidget, ex)
            , interpretSubExpression = \(TagTermExtension tt se) -> do
                (seW, seR) <- se
                n <- get
                modify (1 +)
                let ex = SubExpression $ TagTermExtension tt seR
                    uib _wenv _model =
                      textFieldV_
                        ( case tt of
                            DescriptorTerm txt -> "d." <> txt
                            MetaDescriptorTerm txt -> txt
                        )
                        UpdateExpression
                        [validInput _2]
                    eh _wenv _node model event = case event of
                      UpdateExpression t ->
                        case parseExpr t of
                          Left _ -> [Model $ model & _2 .~ False]
                          Right expr -> case expr of
                            TagTermValue tt' ->
                              case fst model of
                                Just (Right (TagExpression (TagTermExtension _ se'))) ->
                                  [ Model $
                                      model
                                        & _1
                                          .~ ( Just
                                                . Right
                                                . TagExpression
                                                $ TagTermExtension tt' se'
                                             )
                                        & _2 .~ True
                                  ]
                                _nothing ->
                                  [ Model $
                                      model & _1 .~ (Just . Right $ expr)
                                        & (_2 .~ True)
                                  ]
                            _replaceExpression ->
                              [ Model $
                                  model & (_1 .~ (Just . Right $ expr))
                                    & (_2 .~ True)
                              ]
                      _notPossible -> []
                    c = composite "SubExpressionWidget" (plens n) uib eh
                    finalWidget =
                      hstack
                        [ c
                        , label_ "{" [resizeFactor (-1)]
                        , seW
                        , label_ "}" [resizeFactor (-1)]
                        ]
                return (finalWidget, ex)
            }
      , interpretFileTerm = fileTermExpressionWidget
      , interpretTagTerm = \tt ->
          let ex = TagTermValue tt
              uib _ _ =
                textFieldV_
                  ( case tt of
                      DescriptorTerm txt -> "d." <> txt
                      MetaDescriptorTerm txt -> txt
                  )
                  UpdateExpression
                  [validInput _2]
              eh _ _ model event =
                case event of
                  UpdateExpression t -> exprUpdateExpression model t
                  _notPossible -> []
           in do
                n <- get
                let c = composite "TagTermValueWidget" (plens n) uib eh
                modify (1 +)
                return (c, ex)
      , interpretBinaryExpression = \(BinaryOperation (lhsW, lhs) so (rhsW, rhs)) ->
          let ex = BinaryExpression $ BinaryOperation lhs so rhs
              uib _ _ = binaryOpUIB
              eh = binaryOpEventHandler
           in do
                n <- get
                let c = composite "BinaryExpressionWidget" (plens n) uib eh
                    finalWidget = hstack [lhsW, c, rhsW]
                modify (1 +)
                return (finalWidget, ex)
      , interpretTagExpression = \(TagTermExtension tt se) -> do
          (seW, seR) <- se
          n <- get
          let ex = TagExpression $ TagTermExtension tt seR
              uib _wenv _model =
                textFieldV_
                  ( case tt of
                      DescriptorTerm txt -> "d." <> txt
                      MetaDescriptorTerm txt -> txt
                  )
                  UpdateExpression
                  [validInput _2]
              eh _wenv _node model event = case event of
                UpdateExpression t ->
                  case parseExpr t of
                    Left _ -> [Model $ model & _2 .~ False]
                    Right expr -> case expr of
                      TagTermValue tt' ->
                        case fst model of
                          Just (Right (TagExpression (TagTermExtension _ se'))) ->
                            [ Model $
                                model
                                  & _1
                                    .~ ( Just
                                          . Right
                                          . TagExpression
                                          $ TagTermExtension tt' se'
                                       )
                                  & _2 .~ True
                            ]
                          _nothing ->
                            [ Model $
                                model & _1 .~ (Just . Right $ expr)
                                  & (_2 .~ True)
                            ]
                      _replaceExpression ->
                        [ Model $
                            model & (_1 .~ (Just . Right $ expr))
                              & (_2 .~ True)
                        ]
                _notPossible -> []
              c = composite "TagExpressionWidget" (plens n) uib eh
              finalWidget =
                hstack
                  [ c
                  , label_ "{" [resizeFactor (-1)]
                  , seW
                  , label_ "}" [resizeFactor (-1)]
                  ]
          modify (1 +)
          return (finalWidget, ex)
      }
   where
    fileTermExpressionWidget ft@(FileTerm txt) = do
      n <- get
      let ex = FileTermValue ft
          uib _wenv _model = label_ ("p." <> txt) [resizeFactor (-1)] :: WidgetNode QueryBuilderModel QueryBuilderEvent
          eh _wenv _node model event = case event of
            _notPossible -> []
          c = composite "FileTermExpressionWidget" (plens n) uib eh
      return (c, ex)
    exprUpdateExpression ::
      QueryBuilderModel ->
      Text ->
      [EventResponse QueryBuilderModel QueryBuilderEvent TaggerModel TaggerEvent]
    exprUpdateExpression model t = case parseExpr t of
      Left _ -> [Model $ model & _2 .~ False]
      Right expr ->
        [ Model $
            model
              & (_1 .~ (Just . Right $ expr))
              & (_2 .~ True)
        ]
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
    dropdownShowSO so' =
      case so' of
        Union -> "|"
        Intersect -> ""
        Difference -> "!"
    binaryOpUIB =
      dropdownV
        Union
        (\_ so' -> UpdateSetOp so')
        [Union, Intersect, Difference]
        (flip label_ [resizeFactor (-1)] . dropdownShowSO)
        (flip label_ [resizeFactor (-1)] . dropdownShowSO)
    binaryOpEventHandler _wenv _node model event = case event of
      UpdateSetOp so' ->
        case fst model of
          Just (Right (BinaryExpression (BinaryOperation lhs' _ rhs'))) ->
            [ Model $
                model
                  & ( _1
                        .~ (Just . Right . BinaryExpression)
                          (BinaryOperation lhs' so' rhs')
                    )
                  & (_2 .~ True)
            ]
          _others -> []
      _notPossible -> []
    mkExpr t =
      case parseExpr t of
        Left _ -> case parseTagExpr t of
          Left _ -> Nothing
          Right se -> Just . Left $ se
        Right expr -> Just . Right $ expr

type QueryBuilderModel = (Maybe (Either SubExpression Expression), Bool)

data QueryBuilderEvent
  = UpdateExpression Text
  | UpdateSetOp SetOp