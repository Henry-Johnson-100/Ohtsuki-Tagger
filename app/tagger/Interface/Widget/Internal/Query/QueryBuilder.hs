{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant multi-way if" #-}

module Interface.Widget.Internal.Query.QueryBuilder (
  expressionWidget,
  queryEditorTextFieldKey,
) where

import Control.Lens ((^.))
import Control.Monad.Trans.State.Strict (
  State,
  evalState,
  get,
  modify,
 )
import Data.Event (FileSelectionEvent (ClearSelection), QueryEvent (CycleExprSetOpAt, PushExpression, RingProduct, RunQuery, UpdateExpression, UpdateSubExpression), TaggerEvent (DoFileSelectionEvent, DoQueryEvent, ToggleQueryEditMode, Unit), anonymousEvent)
import Data.Model (TaggerModel, fileSelectionModel, queryEditMode, queryModel)
import Data.Model.Lens (expression, input)
import Data.Model.Shared.Lens (HasText (text))
import Data.Monoid (Sum (..))
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (Descriptor (Descriptor), concreteTagDescriptor, descriptor)
import Interface.Theme
import Interface.Widget.Internal.Core
import Monomer
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Engine

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

tShowSetOp :: SetOp -> Text
tShowSetOp Union = "|"
tShowSetOp Intersect = "&"
tShowSetOp Difference = "!"

queryEditorTextFieldKey :: Text
queryEditorTextFieldKey = "queryEditorTextField"

expressionWidget _ = label "Please Understand"

-- expressionWidget :: Expression -> TaggerWidget
-- expressionWidget expr =
--   keystroke
--     [ ("Ctrl-e", ToggleQueryEditMode)
--     , ("Ctrl-u", DoFileSelectionEvent ClearSelection)
--     , ("Enter", DoQueryEvent PushExpression)
--     , ("Shift-Enter", DoQueryEvent RunQuery)
--     ]
--     $ vstack
--       [ hstack
--           [ toggleButton_ "Exit" queryEditMode [resizeFactor (-1)]
--           ]
--       , queryEditorWidget expr
--       , box_ [alignMiddle, alignBottom]
--           . withNodeKey queryEditorTextFieldKey
--           $ textField (fileSelectionModel . queryModel . input . text)
--       ]

-- queryEditorWidget :: Expression -> TaggerWidget
-- queryEditorWidget =
--   vscroll_ [wheelRate 50]
--     . withStyleBasic [border 1 green]
--     . box_ [alignCenter, alignMiddle]
--     . snd
--     . flip evalState 1
--     . runExpressionInterpreter expressionWidgetBuilder

-- expressionWidgetBuilder ::
--   ExpressionInterpreter
--     (State Int)
--     (SubExpression, Int -> TaggerWidget)
--     (Expression, TaggerWidget)
-- expressionWidgetBuilder =
--   ExpressionInterpreter
--     { subExpressionInterpreter = subExpressionWidgetBuilder
--     , interpretFileTerm = \(FileTerm t) -> do
--         pos <- get
--         let ex = FileTermValue . FileTerm $ t
--             w =
--               dragDropUpdater pos ex $
--                 label_ ("p." <> t) [resizeFactor (-1)]
--         modify (1 +)
--         return (ex, w)
--     , interpretTagTerm = \tt -> do
--         pos <- get
--         let ex = TagTermValue tt
--             w =
--               dragDropUpdater pos ex $
--                 label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
--         modify (1 +)
--         return (ex, w)
--     , interpretBinaryExpression =
--         \bn@(BinaryOperation lhst@(lhs, lhsw) so rhst@(rhs, rhsw)) -> do
--           pos <- get
--           let mkBin =
--                 let ex = BinaryExpression $ fmap fst bn
--                     mkEitherOperandWidget e =
--                       withStyleHover [border 1 yuiOrange] $
--                         hstack
--                           [ snd . either id id $ e
--                           , withStyleBasic [textSize 5]
--                               . box_ [alignTop, alignRight]
--                               $ styledButton_
--                                 [resizeFactor (-1)]
--                                 "x"
--                                 ( DoQueryEvent
--                                     . UpdateExpression pos
--                                     $ ( case e of
--                                           Right _ -> lhs
--                                           Left _ -> rhs
--                                       )
--                                 )
--                           ]
--                     w =
--                       hstack
--                         [ mkEitherOperandWidget . Left $ lhst
--                         , dropTarget_
--                             (DoQueryEvent . UpdateExpression pos)
--                             [dropTargetStyle [border 1 yuiOrange]]
--                             $ styledButton_
--                               [resizeFactor (-1)]
--                               (tShowSetOp so)
--                               (DoQueryEvent $ CycleExprSetOpAt pos)
--                         , if ( case rhs of
--                                 BinaryExpression _ -> True
--                                 _notNested -> False
--                              )
--                             then
--                               hstack
--                                 [ label_ "(" [resizeFactor (-1)]
--                                 , mkEitherOperandWidget . Right $ rhst
--                                 , label_ ")" [resizeFactor (-1)]
--                                 ]
--                             else mkEitherOperandWidget . Right $ rhst
--                         ]
--                  in ( ex
--                     , draggable ex $
--                         if ex == aid
--                           then
--                             dragDropUpdater pos (aid :: Expression) $
--                               label_ "∅" [resizeFactor (-1)]
--                           else w
--                     )
--               mkWidget = case so of
--                 Union ->
--                   if
--                       | lhs == aid -> (rhs, rhsw)
--                       | rhs == aid -> (lhs, lhsw)
--                       | otherwise -> mkBin
--                 Intersect ->
--                   if
--                       | lhs == mid -> (rhs, rhsw)
--                       | rhs == mid -> (lhs, lhsw)
--                       | otherwise -> mkBin
--                 Difference ->
--                   if
--                       | lhs == aid ->
--                         ( aid
--                         , dragDropUpdater pos (aid :: Expression) $
--                             label_ "∅" [resizeFactor (-1)]
--                         )
--                       | rhs == aid -> (lhs, lhsw)
--                       | otherwise -> mkBin
--           modify (1 +)
--           return mkWidget
--     , interpretTagExpression = \(TagTermExtension tt se') -> do
--         pos <- get
--         let (se, sew) = evalState se' 1
--             ex = TagExpression $ TagTermExtension tt se
--             w =
--               dragDropUpdater pos ex $
--                 hstack
--                   [ label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
--                   , hstack
--                       [ label_ "{" [resizeFactor (-1)]
--                       , sew pos
--                       , label_ "}" [resizeFactor (-1)]
--                       ]
--                   ]
--         modify (1 +)
--         return (ex, w)
--     }
--  where
--   dragDropUpdater n dragExpr =
--     dropTarget_ (DoQueryEvent . UpdateExpression n) [dropTargetStyle [border 1 yuiOrange]]
--       . draggable dragExpr

-- relativeToParent ::
--   (Eq b, ExpressionIndex b) =>
--   b ->
--   b ->
--   Maybe (Sum Int)
-- relativeToParent parentExpr childExpr = do
--   let parentSize = fst . head . flatten $ parentExpr
--   relChildPos <- fst <$> index childExpr parentExpr
--   return . Sum $ relChildPos - parentSize

-- subExpressionWidgetBuilder ::
--   SubExpressionInterpreter
--     -- State tracking the index of the SubExpression as it is traversed
--     (State Int)
--     -- Where the snd function is a reader taking the parent Expression's index
--     (SubExpression, Int -> TaggerWidget)
-- subExpressionWidgetBuilder =
--   SubExpressionInterpreter
--     { interpretSubTag = \tt -> do
--         sePos <- get
--         let se = SubTag tt
--             w exprPos =
--               dragDropUpdater exprPos sePos se $
--                 label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
--         modify (1 +)
--         return (se, w)
--     , interpretBinarySubExpression =
--         \bn@(BinaryOperation (_, lhsw) so (rhs, rhsw)) -> do
--           _pos <- get
--           let w exprPos =
--                 hstack
--                   [ lhsw exprPos
--                   , label_ (tShowSetOp so) [resizeFactor (-1)]
--                   , if (case rhs of BinarySubExpression _ -> True; _notNested -> False)
--                       then
--                         hstack
--                           [ label_ "(" [resizeFactor (-1)]
--                           , rhsw exprPos
--                           , label_ ")" [resizeFactor (-1)]
--                           ]
--                       else rhsw exprPos
--                   ]
--           modify (1 +)
--           return (BinarySubExpression $ fmap fst bn, w)
--     , interpretSubExpression = \(TagTermExtension tt se') -> do
--         (se, sew) <- se'
--         sePos <- get
--         let ttese = SubExpression $ TagTermExtension tt se
--             w exprPos =
--               draggable ttese $
--                 hstack
--                   [ draggable tt $ label_ (tt ^. tagTermPatternL) [resizeFactor (-1)]
--                   , label_ "{" [resizeFactor (-1)]
--                   , sew exprPos
--                   , label_ "}" [resizeFactor (-1)]
--                   ]
--         modify (1 +)
--         return (ttese, w)
--     }
--  where
--   dragDropUpdater exprPos sePos se =
--     dropTarget_
--       (DoQueryEvent . UpdateSubExpression exprPos sePos)
--       [dropTargetStyle [border 1 yuiOrange]]
--       . draggable se