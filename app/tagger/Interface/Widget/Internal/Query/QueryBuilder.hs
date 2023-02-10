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

import Control.Applicative (liftA)
import Control.Lens hiding (Magma, index, (#))
import Control.Monad (ap)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (
  StateT,
  get,
  modify,
  runStateT,
 )
import Data.Data (Typeable)
import Data.Event (TaggerEvent)
import Data.Model (TaggerModel, expression, fileSelectionModel, queryModel)
import Data.Text (Text)
import qualified Data.Text as T
import Interface.Widget.Internal.Core
import Monomer
import Text.TaggerQL.Expression.AST

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

{- |
 A class for monadic computations that can be incremented.
 Additionally, the current increment can be retrieved.
-}
class IncrementableM m where
  incr :: m ()
  getIncr :: m Int

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

{- |
 A state monad transformer for 1-indexed incremental operations.
-}
newtype CounterT m a = CounterT (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => IncrementableM (CounterT m) where
  incr :: Monad m => CounterT m ()
  incr = CounterT (modify (1 +))
  getIncr :: Monad m => CounterT m Int
  getIncr = CounterT get

runCounter :: Monad m => CounterT m a -> m (a, Int)
runCounter (CounterT s) = runStateT s 1

-- testquery:
-- o%yui {(gym_clothes | swimsuit | kimono){blue} hair{r%bow}! (suggestive | sweetie_cute_new_year {+})}

queryEditorTextFieldKey :: Text
queryEditorTextFieldKey = "queryEditorTextField"

expressionWidget :: QueryExpression -> TaggerWidget
expressionWidget = box_ [alignCenter] . buildQueryExpressionWidget

{- |
 A class for types that can be safely coerced to a @WithIndex m a@
-}
class CoerceWithIndex c where
  coerceWithIndex :: Monad m => c m a -> WithIndex m a

instance CoerceWithIndex CounterT where
  coerceWithIndex :: Monad m => CounterT m a -> WithIndex m a
  coerceWithIndex = constructWithIndexM

{- |
 Apply a pure function to some leaf of a structure that is in a monadic context
 by traversing its structure and surfacing a continuation at the appropriate index.
-}
newtype WithIndex m a = WithIndex
  {runWithIndex :: Int -> (a -> a) -> CounterT m a}

instance CoerceWithIndex WithIndex where
  coerceWithIndex :: WithIndex m a -> WithIndex m a
  coerceWithIndex = id

instance Monad m => Functor (WithIndex m) where
  fmap :: Monad m => (a -> b) -> WithIndex m a -> WithIndex m b
  fmap = liftA

instance Monad m => Applicative (WithIndex m) where
  pure :: Monad m => a -> WithIndex m a
  pure = return
  (<*>) ::
    Monad m =>
    WithIndex m (a -> b) ->
    WithIndex m a ->
    WithIndex m b
  (<*>) = ap

instance Monad m => Monad (WithIndex m) where
  return :: Monad m => a -> WithIndex m a
  return x = WithIndex $ \i f -> do
    count <- getIncr
    pure . (if count == i then f else id) $ x
  (>>=) ::
    Monad m =>
    WithIndex m a ->
    (a -> WithIndex m b) ->
    WithIndex m b
  -- Retrieves the pure value from the left and returns a new traverser for the same
  -- index but with a continuation for the type of (f x)
  x >>= f = WithIndex $ \i g -> do
    x' <- runWithIndex x i id
    runWithIndex (f x') i g

instance MonadTrans WithIndex where
  lift :: Monad m => m a -> WithIndex m a
  lift m = WithIndex $ \i f -> do
    x <- lift m
    count <- getIncr
    pure . (if count == i then f else id) $ x

instance Monad m => IncrementableM (WithIndex m) where
  incr :: Monad m => WithIndex m ()
  incr = WithIndex $ \_ _ -> incr
  getIncr :: Monad m => WithIndex m Int
  getIncr = WithIndex $ \_ _ -> getIncr

{- |
 Lift a 'CounterT` to a 'WithIndex`

 This function exposes the inner @CounterT m@ to the pure value 'a`.

 For example, for a @WithIndex (State s) a@
 this function allows for the value of type 'a` to interact with
  the stateful environment 's`.
-}
constructWithIndexM :: Monad m => CounterT m a -> WithIndex m a
constructWithIndexM c = WithIndex $ \i f -> do
  c' <- c
  count <- getIncr
  pure . (if count == i then f else id) $ c'

{- |
 Just extract the pure 'CounterT` from a 'WithIndex`
-}
ignoreIndex :: WithIndex m a -> CounterT m a
ignoreIndex w = runWithIndex w 0 id

withTagExpressionIndex ::
  (Monad f, CoerceWithIndex c, Rng (c f b), Magma (c f b)) =>
  TagExpression (c f b) ->
  Int ->
  (b -> b) ->
  f b
withTagExpressionIndex te i f =
  fmap fst
    . runCounter
    $ runWithIndex
      ( coerceWithIndex
          . evaluateTagExpressionR (#)
          $ te
      )
      i
      f

buildQueryExpressionWidget :: QueryExpression -> TaggerWidget
buildQueryExpressionWidget qe =
  let qeWithIndex =
        runIdentity $
          withRngExpressionIndex (fmap qewbLeaf . runQueryExpression $ qe) 0 id
   in qeWithIndex ^. widget

withRngExpressionIndex ::
  (Monad f, CoerceWithIndex c, Rng (c f b)) =>
  RingExpression (c f b) ->
  Int ->
  (b -> b) ->
  f b
withRngExpressionIndex rex i f =
  fmap fst . runCounter $ runWithIndex (coerceWithIndex . evaluateRing $ rex) i f

buildTagExpressionWidgetState ::
  TagExpression (DTerm Pattern) ->
  CounterT
    Identity
    (ExpressionWidgetState (TagExpression (DTerm Pattern)))
buildTagExpressionWidgetState te =
  ignoreIndex
    . WithIndex
    $ withTagExpressionIndex (fmap tewbLeaf te)

{- |
 A generic form of 'TagExpressionWidgetBuilder` for deriving newtype instances.
-}
newtype TagExpressionWidgetBuilderG m a
  = TagExpressionWidgetBuilderG
      (WithIndex m a)
  deriving (Functor, Applicative, Monad, MonadTrans, CoerceWithIndex, IncrementableM)

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
  (+.) ::
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern)))
  (+.) x y = tewbBinHelper (+.) (tewbRngWidget "|") x y <&> nestedOperand .~ True
  (*.) ::
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern)))
  (*.) x y = tewbBinHelper (*.) (tewbRngWidget "&") x y <&> nestedOperand .~ True
  (-.) ::
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern))) ->
    TagExpressionWidgetBuilderG
      (CounterT Identity)
      (ExpressionWidgetState (TagExpression (DTerm Pattern)))
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
  ( HasAccumExpression s1 t1
  , HasAccumExpression s2 t2
  , MonadTrans t3
  , Monad m
  , Monad (t3 m)
  , IncrementableM m
  , IncrementableM (t3 m)
  , Eq e
  , Typeable e
  ) =>
  (t1 -> t2 -> e) ->
  (Int -> Int -> s1 -> s2 -> TaggerWidget) ->
  t3 m s1 ->
  t3 m s2 ->
  t3 m (ExpressionWidgetState e)
tewbBinHelper binComb wf lhs rhs = do
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
tewbLeaf d = do
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
      (WithIndex m a)
  deriving (Functor, Applicative, Monad, MonadTrans, CoerceWithIndex, IncrementableM)

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
  ( HasNestedOperand s1 Bool
  , HasWidget s2 (WidgetNode TaggerModel TaggerEvent)
  , HasWidget s1 (WidgetNode TaggerModel TaggerEvent)
  , HasAccumExpression s2 t1
  , HasAccumExpression s1 t2
  , Monad m
  , IncrementableM m
  , Eq e
  , Typeable e
  ) =>
  (t1 -> t2 -> e) ->
  Text ->
  m s2 ->
  m s1 ->
  m (ExpressionWidgetState e)
qewbRngHelper c l lhs rhs = do
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
        FileLeaf pat -> do
          count <- getIncr
          incr
          pure $
            ExpressionWidgetState
              ( draggable qe $
                  label_ ("p." <> patternText pat) [resizeFactor (-1)]
              )
              qe
              False
        TagLeaf te -> do
          teews <-
            QueryExpressionWidgetBuilderG
              . constructWithIndexM
              . buildTagExpressionWidgetState
              $ te
          incr
          pure $
            ExpressionWidgetState
              (draggable qe $ teews ^. widget)
              qe
              False

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