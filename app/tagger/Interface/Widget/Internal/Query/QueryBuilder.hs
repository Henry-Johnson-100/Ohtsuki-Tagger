{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Interface.Widget.Internal.Query.QueryBuilder (
  expressionWidget,
) where

import Control.Lens ((^.))
import Control.Monad.Trans.State.Strict (
  State,
  evalState,
  get,
  modify,
 )
import Data.Event (TaggerEvent)
import Data.Model (TaggerModel)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import Monomer
import Text.TaggerQL.Expression.AST (
  BinaryOperation (BinaryOperation),
  Expression,
  FileTerm (FileTerm),
  TagTermExtension (TagTermExtension),
  tagTermPatternL,
 )
import Text.TaggerQL.Expression.Engine (
  ExpressionInterpreter (..),
  SubExpressionInterpreter (..),
  runExpressionInterpreter,
 )

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

tShowSetOp :: SetOp -> Text
tShowSetOp Union = "|"
tShowSetOp Intersect = "&"
tShowSetOp Difference = "!"

type IsNestedExpression = Bool

expressionWidget :: Expression -> TaggerWidget
expressionWidget =
  box_ [alignTop, alignLeft]
    . snd
    . flip evalState 1
    . runExpressionInterpreter expressionWidgetBuilder

expressionWidgetBuilder ::
  ExpressionInterpreter
    (State Int)
    (IsNestedExpression, Int -> TaggerWidget)
    (IsNestedExpression, TaggerWidget)
expressionWidgetBuilder =
  ExpressionInterpreter
    { subExpressionInterpreter = subExpressionWidgetBuilder
    , interpretFileTerm = \(FileTerm t) -> do
        _pos <- get
        let w = label_ ("p." <> t) [resizeFactor (-1)]
        modify (1 +)
        return (False, w)
    , interpretTagTerm = \tt -> do
        _pos <- get
        let w = label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
        modify (1 +)
        return (False, w)
    , interpretBinaryExpression =
        \(BinaryOperation (_, lhsw) so (rhsIsNested, rhsw)) -> do
          _pos <- get
          let w =
                hstack
                  [ lhsw
                  , label_ (tShowSetOp so) [resizeFactor (-1)]
                  , if rhsIsNested
                      then
                        hstack
                          [ label_ "(" [resizeFactor (-1)]
                          , rhsw
                          , label_ ")" [resizeFactor (-1)]
                          ]
                      else rhsw
                  ]
          modify (1 +)
          return (True, w)
    , interpretTagExpression = \(TagTermExtension tt se') -> do
        pos <- get
        let (_, se) = evalState se' 1
            w =
              hstack
                [ label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
                , hstack
                    [ label_ "{" [resizeFactor (-1)]
                    , se pos
                    , label_ "}" [resizeFactor (-1)]
                    ]
                ]
        modify (1 +)
        return (False, w)
    }

subExpressionWidgetBuilder ::
  SubExpressionInterpreter
    -- State tracking the index of the SubExpression as it is traversed
    (State Int)
    -- Where the snd function is a reader taking the parent Expression's index
    (IsNestedExpression, Int -> TaggerWidget)
subExpressionWidgetBuilder =
  SubExpressionInterpreter
    { interpretSubTag = \tt -> do
        _sepos <- get
        let w _exprPos = label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
        modify (1 +)
        return (False, w)
    , interpretBinarySubExpression =
        \(BinaryOperation (_, lhsw) so (rhsIsNested, rhsw)) -> do
          _pos <- get
          let w exprPos =
                hstack
                  [ lhsw exprPos
                  , label_ (tShowSetOp so) [resizeFactor (-1)]
                  , if rhsIsNested
                      then
                        hstack
                          [ label_ "(" [resizeFactor (-1)]
                          , rhsw exprPos
                          , label_ ")" [resizeFactor (-1)]
                          ]
                      else rhsw exprPos
                  ]
          modify (1 +)
          return (True, w)
    , interpretSubExpression = \(TagTermExtension tt se') -> do
        (_, sew) <- se'
        _sepos <- get
        let w exprPos =
              hstack
                [ label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
                , label_ "{" [resizeFactor (-1)]
                , sew exprPos
                , label_ "}" [resizeFactor (-1)]
                ]
        modify (1 +)
        return (False, w)
    }