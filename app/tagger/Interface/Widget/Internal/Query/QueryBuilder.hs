{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Redundant multi-way if" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module Interface.Widget.Internal.Query.QueryBuilder (
  expressionWidget,
  queryEditorTextFieldKey,
) where

import Control.Lens hiding (Magma, index, (#))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (
  StateT,
  get,
  modify,
  runStateT,
 )
import Data.Event (TaggerEvent)
import Data.Model (TaggerModel)
import Data.Text (Text)
import qualified Data.Text as T
import Interface.Widget.Internal.Core
import Monomer
import Text.TaggerQL.Expression.AST

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

{- |
 A stateful accumulator that builds a total widget out of some given expression e.
-}
data ExpressionWidgetState e = ExpressionWidgetState
  { _expressionwidgetstateWidget :: TaggerWidget
  , _expressionwidgetstateAccumExpression :: e
  , _expressionwidgetstateNestedOperand :: Bool
  }

makeLensesWith abbreviatedFields ''ExpressionWidgetState

expressionWidgetState w e = ExpressionWidgetState w e False

{- |
 A state monad transformer for 1-indexed incremental operations.
-}
newtype CounterT m a = CounterT (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runCounter :: Monad m => CounterT m a -> m (a, Int)
runCounter (CounterT s) = runStateT s 1

incr :: Monad m => CounterT m ()
incr = CounterT (modify (1 +))

getCount :: Monad m => CounterT m Int
getCount = CounterT get

newtype TagExpressionWidget = TagExpressionWidget
  { runTagExpressionWidget ::
      CounterT
        -- This inner count is the index of whatever query leaf the widget interpreter
        -- is on
        (CounterT Identity)
        (ExpressionWidgetState (TagExpression (DTerm Pattern)))
  }

instance Rng TagExpressionWidget where
  (+.) :: TagExpressionWidget -> TagExpressionWidget -> TagExpressionWidget
  x +. y = mkRngTagExprWidget x y "|" (+.)
  (*.) :: TagExpressionWidget -> TagExpressionWidget -> TagExpressionWidget
  x *. y = mkRngTagExprWidget x y "&" (*.)
  (-.) :: TagExpressionWidget -> TagExpressionWidget -> TagExpressionWidget
  x -. y = mkRngTagExprWidget x y "!" (-.)

{- |
 Helper function for defining Rng instances.
-}
mkRngTagExprWidget ::
  TagExpressionWidget ->
  TagExpressionWidget ->
  Text ->
  ( TagExpression (DTerm Pattern) ->
    TagExpression (DTerm Pattern) ->
    TagExpression (DTerm Pattern)
  ) ->
  TagExpressionWidget
mkRngTagExprWidget (TagExpressionWidget lhs) (TagExpressionWidget rhs) l c =
  TagExpressionWidget $ do
    x <- lhs
    y <- rhs
    leafCount <- lift getCount
    teCount <- getCount
    incr
    pure $ f leafCount teCount x y
 where
  f li i x y =
    ExpressionWidgetState
      ( withStyleBasic [paddingT 3, paddingR 3, paddingB 3] $
          vstack
            [ x ^. widget
            , hstack
                [ tooltip_ (T.pack . show $ (li, i)) [tooltipDelay 500] consLabel
                , spacer
                , (if y ^. nestedOperand then mkParens else id) $ y ^. widget
                ]
            ]
      )
      (c (x ^. accumExpression) (y ^. accumExpression))
      True
   where
    mkParens w = withStyleBasic [border 1 black, padding 3] w

    consLabel = label_ l [resizeFactor (-1)]

-- testquery:
-- o%yui {(gym_clothes | swimsuit | kimono){blue} hair{r%bow}! (suggestive | sweetie_cute_new_year {+})}

instance Magma TagExpressionWidget where
  (#) :: TagExpressionWidget -> TagExpressionWidget -> TagExpressionWidget
  (TagExpressionWidget x') # (TagExpressionWidget y') = TagExpressionWidget $ do
    x <- x'
    y <- y'
    leafCount <- lift getCount
    teCount <- getCount
    incr
    pure $ f leafCount teCount x y
   where
    f li i (x :: ExpressionWidgetState (TagExpression (DTerm Pattern))) y =
      ExpressionWidgetState
        ( withStyleBasic [border 1 black, padding 3] $
            vstack
              [ box_ [alignTop] $ x ^. widget
              , tooltip_ (T.pack . show $ (li, i)) [tooltipDelay 500]
                  . withStyleBasic [paddingB 3]
                  $ vstack
                    [ withStyleBasic [paddingB 5] $
                        separatorLine_ [resizeFactor (-1)]
                    , separatorLine_ [resizeFactor (-1)]
                    ]
              , box_ [alignTop, alignCenter] $ y ^. widget
              ]
        )
        ((x ^. accumExpression) # (y ^. accumExpression))
        False

dTermPatternWidget ::
  DTerm Pattern ->
  TagExpressionWidget
dTermPatternWidget d = TagExpressionWidget $ do
  leafCount <- lift getCount
  teCount <- getCount
  incr
  pure $ f leafCount teCount d
 where
  f li i d' = expressionWidgetState w (pure d')
   where
    dTermLabelText = case d' of
      DTerm (Pattern t) -> "d." <> t
      DMetaTerm (Pattern t) -> t
      _synonymNotMatched -> T.pack . show $ d'

    w =
      tooltip_ (T.pack . show $ (li, i)) [tooltipDelay 500] $
        label_ dTermLabelText [resizeFactor (-1)]

tagExpressionWidget :: TagExpression (DTerm Pattern) -> TagExpressionWidget
tagExpressionWidget = evaluateTagExpressionR (#) . fmap dTermPatternWidget

queryEditorTextFieldKey :: Text
queryEditorTextFieldKey = "queryEditorTextField"

newtype QueryExpressionWidget = QueryExpressionWidget
  {runQueryExpressionWidget :: (TaggerWidget, QueryExpression)}

instance Rng QueryExpressionWidget where
  x +. y = bimapQEW (\xw yw -> hstack [xw, label_ "|" [resizeFactor (-1)], yw]) (+.) x y

  x *. y = bimapQEW (\xw yw -> hstack [xw, label_ "&" [resizeFactor (-1)], yw]) (*.) x y

  x -. y = bimapQEW (\xw yw -> hstack [xw, label_ "!" [resizeFactor (-1)], yw]) (-.) x y

bimapQEW
  cw
  cr
  (QueryExpressionWidget (xw, xqe))
  (QueryExpressionWidget (yw, yqe)) =
    QueryExpressionWidget (xw `cw` yw, xqe `cr` yqe)

expressionWidget :: QueryExpression -> TaggerWidget
expressionWidget =
  box_ [alignCenter]
    . fst
    . runQueryExpressionWidget
    . evaluateRing
    . fmap queryLeafWidget
    . runQueryExpression

-- These are just very basic labels for now.
queryLeafWidget :: QueryLeaf -> QueryExpressionWidget
queryLeafWidget ql =
  let qe = QueryExpression . Ring $ ql
   in case ql of
        FileLeaf pat ->
          let w = label_ ("p." <> patternText pat) [resizeFactor (-1)]
           in QueryExpressionWidget (w, qe)
        TagLeaf te ->
          let r = tagExpressionWidget te
           in QueryExpressionWidget
                ( ( fst
                      -- this has to be changed at some point THOUGH
                      . runIdentity
                      . runCounter
                      -- discard the tag expression index because it doesn't matter
                      -- after this compotatoation is finished.
                      . fmap fst
                      . runCounter
                      . runTagExpressionWidget
                      $ r
                  )
                    ^. widget
                , qe
                )

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