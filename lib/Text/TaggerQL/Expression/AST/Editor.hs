{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Expression.AST.Editor (
  findQueryExpression,
  withQueryExpression,
  findTagExpression,
  withTagExpression,

  -- * Structure Specific Operations
  nextRingOperator,
  dropLeftTree,
  dropRightTree,
  duplicateRing,
  onTagLeaf,

  -- * Counter
  CounterT,
  incr,
  getIncr,
  runCounter,
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State.Strict (StateT (..), get, modify)
import Data.Bifunctor
import Data.Functor.Identity (runIdentity)
import Text.TaggerQL.Expression.AST

{- |
 Modifies the TagExpression in a TagLeaf of a QueryExpression if it is a ring value
 and not a binary operation.
-}
onTagLeaf ::
  QueryExpression ->
  (TagQueryExpression -> TagQueryExpression) ->
  QueryExpression
onTagLeaf fqe'@(TraversableQueryExpression fqe) f = case fqe of
  Node e -> TraversableQueryExpression . Node $ bimap (second f) (second f) e
  _notNode -> fqe'

{- |
 Cycles 'RingExpression` constructors for a single binary expression.
-}
nextRingOperator :: RingExpression a -> RingExpression a
nextRingOperator r = case r of
  Node _ -> r
  Edge lft so lft' -> flip (Edge lft) lft' $
    case so of
      Addition -> Multiplication
      Multiplication -> Subtraction
      Subtraction -> Addition

{- |
 Drops the right operand from a 'RingExpression` if there is one.
-}
dropRightTree :: LabeledFreeTree l a -> LabeledFreeTree l a
dropRightTree r = case r of
  Node _ -> r
  Edge x _ _ -> x

{- |
 Drops the left operand from a 'RingExpression` if there is one.
-}
dropLeftTree :: LabeledFreeTree l a -> LabeledFreeTree l a
dropLeftTree r = case r of
  Node _ -> r
  Edge _ _ x -> x

{- |
 Duplicate a ring in place by intersecting it with itself.
-}
duplicateRing :: RingExpression a -> RingExpression a
duplicateRing r = r *. r

{- |
 A state monad transformer for 1-indexed incremental operations.

 More specifically, this type is the underlying monad used for editing and indexing
 expressions of various types. This type is exposed to provide a method for
 building widgets out of an expression. Where the widget is annotated with an index
 at its specific point in computation so that it can send events that target
 the correct term of an expression.
-}
newtype CounterT m a = CounterT (StateT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

incr :: Monad m => CounterT m ()
incr = CounterT (modify (1 +))

getIncr :: Monad m => CounterT m Int
getIncr = CounterT get

runCounter :: Monad m => CounterT m a -> m (a, Int)
runCounter (CounterT s) = runStateT s 1

{- |
 Has a bind function 'bindFinder` but no monadic identity.
-}
newtype FinderT m a = FinderT (CounterT m (a, Int, Maybe a))

instance (Rng a, Monad m) => Rng (FinderT m a) where
  (+.) :: (Rng a, Monad m) => FinderT m a -> FinderT m a -> FinderT m a
  (+.) = finderBinHelper (+.)
  (*.) :: (Rng a, Monad m) => FinderT m a -> FinderT m a -> FinderT m a
  (*.) = finderBinHelper (*.)
  (-.) :: (Rng a, Monad m) => FinderT m a -> FinderT m a -> FinderT m a
  (-.) = finderBinHelper (-.)

instance (Magma a, Monad m) => Magma (FinderT m a) where
  (∙) :: (Magma a, Monad m) => FinderT m a -> FinderT m a -> FinderT m a
  (∙) = finderBinHelper (∙)

finderBinHelper ::
  Monad m =>
  (a -> a -> a) ->
  FinderT m a ->
  FinderT m a ->
  FinderT m a
finderBinHelper c (FinderT x) (FinderT y) = FinderT $ do
  (x', _, xm) <- x
  (y', yix, ym) <- y
  count <- getIncr
  incr
  let comb = x' `c` y'
  pure (comb, yix, xm <|> ym <|> (if count == yix then Just comb else Nothing))

mkFinder :: Monad m => Int -> a -> FinderT m a
mkFinder n x = FinderT $ do
  count <- getIncr
  incr
  pure (x, n, if n == count then Just x else Nothing)

evalFinder :: Monad f => FinderT f a -> f (Maybe a)
evalFinder (FinderT c) =
  let r (_, _, mr) = mr
   in fmap (r . fst) . runCounter $ c

{- |
 Bind the pure value in a 'FinderT` to a function that produces a 'FinderT`
-}
bindFinder :: Monad m => FinderT m a -> (a -> FinderT m a) -> FinderT m a
bindFinder (FinderT x) f = FinderT $ do
  (x', n, mx) <- x
  (r, _, mr) <- (\(FinderT z) -> z) . f $ x'
  pure (r, n, mx <|> mr)

{- |
 A has a bind function 'BindEditor` but no monadic identity.
-}
newtype EditorT m a = EditorT (CounterT m (a, Int, a -> a))

instance (Rng a, Monad m) => Rng (EditorT m a) where
  (+.) :: (Rng a, Monad m) => EditorT m a -> EditorT m a -> EditorT m a
  (+.) = editorTBinHelper (+.)
  (*.) :: (Rng a, Monad m) => EditorT m a -> EditorT m a -> EditorT m a
  (*.) = editorTBinHelper (*.)
  (-.) :: (Rng a, Monad m) => EditorT m a -> EditorT m a -> EditorT m a
  (-.) = editorTBinHelper (-.)

instance (Magma a, Monad m) => Magma (EditorT m a) where
  (∙) :: (Magma a, Monad m) => EditorT m a -> EditorT m a -> EditorT m a
  (∙) = editorTBinHelper (∙)

editorTBinHelper ::
  Monad m =>
  (a1 -> a2 -> a2) ->
  EditorT m a1 ->
  EditorT m a2 ->
  EditorT m a2
editorTBinHelper c (EditorT x) (EditorT y) = EditorT $ do
  (x', _, _) <- x
  (y', n, f) <- y
  count <- getIncr
  incr
  let comb = x' `c` y'
  pure (if count == n then f comb else comb, n, f)

mkEditor :: Monad m => Int -> (a -> a) -> a -> EditorT m a
mkEditor n f x = EditorT $ do
  count <- getIncr
  incr
  pure (if count == n then f x else x, n, f)

evalEditor :: Monad f => EditorT f b -> f b
evalEditor (EditorT c) =
  let r (x, _, _) = x
   in r . fst <$> runCounter c

{- |
 Bind the pure value in an 'EditorT` to a function that produces an 'EditorT`.
-}
bindEditor :: Monad m => EditorT m a1 -> (a1 -> EditorT m a2) -> EditorT m a2
bindEditor (EditorT x) f = EditorT $ do
  (x', _, _) <- x
  (r, n, g) <- (\(EditorT z) -> z) . f $ x'
  pure (r, n, g)

findQueryExpression :: Int -> QueryExpression -> Maybe QueryExpression
findQueryExpression n =
  runIdentity
    . evalFinder
    . evaluateRingExpression
    . mkFinderRing
 where
  mkFinderRing =
    fmap
      ( either
          ( \(x, y) ->
              bindFinder (evaluateRingExpression . mkFinderRing $ x) $
                mkFinder n . (`distributeTagExpression` y)
          )
          (mkFinder n . TraversableQueryExpression . Node . Right)
      )
      . runTraversableQueryExpression

{- |
 Modify the 'QueryExpression` at the given index.
-}
withQueryExpression ::
  Int ->
  QueryExpression ->
  (QueryExpression -> QueryExpression) ->
  QueryExpression
withQueryExpression n qe f =
  runIdentity
    . evalEditor
    . evaluateRingExpression
    . mkEditorRing
    $ qe
 where
  mkEditorRing =
    fmap
      ( either
          ( \(x, y) ->
              bindEditor (evaluateRingExpression . mkEditorRing $ x) $
                mkEditor n f . (`distributeTagExpression` y)
          )
          (mkEditor n f . TraversableQueryExpression . Node . Right)
      )
      . runTraversableQueryExpression

findTagExpression ::
  Int ->
  FreeDisjunctMonad RingExpression MagmaExpression a ->
  Maybe (FreeDisjunctMonad RingExpression MagmaExpression a)
findTagExpression n =
  runIdentity
    . evalFinder
    . evaluateFreeCompoundExpression evaluateRingExpression evaluateMagmaExpression
    . fmap (mkFinder n . pure)

{- |
 Modify the 'TagExpression` at the given index.
-}
withTagExpression ::
  Int ->
  FreeDisjunctMonad RingExpression MagmaExpression a ->
  ( FreeDisjunctMonad RingExpression MagmaExpression a ->
    FreeDisjunctMonad RingExpression MagmaExpression a
  ) ->
  FreeDisjunctMonad RingExpression MagmaExpression a
withTagExpression n te f =
  runIdentity
    . evalEditor
    . evaluateFreeCompoundExpression evaluateRingExpression evaluateMagmaExpression
    . fmap (mkEditor n f . pure)
    $ te