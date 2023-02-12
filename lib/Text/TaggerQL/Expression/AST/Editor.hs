{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune #-}

module Text.TaggerQL.Expression.AST.Editor (
  findQueryExpression,
  withQueryExpression,
  findTagExpression,
  withTagExpression,

  -- * Structure Specific Operations
  flipRingExpression,
  nextRingOperator,
  dropLeftRing,
  dropRightRing,
  duplicateRing,
  onTagRing,
  onTagLeaf,
  cutMagmaExpression,
  onTagMagma,
  distributeTagExpression,
  (<-#),

  -- * Counter
  CounterT,
  incr,
  getIncr,
  runCounter,
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State.Strict (StateT (..), get, modify)
import Data.Functor.Identity (runIdentity)
import Text.TaggerQL.Expression.AST (
  DTerm,
  Magma (..),
  MagmaExpression (..),
  Pattern,
  QueryExpression (..),
  QueryLeaf (..),
  RingExpression (..),
  Rng (..),
  TagExpression (..),
  evaluateRing,
  evaluateTagExpressionR,
 )

{- |
 A function handling left-distribution of a 'TagExpression` into a 'QueryExpression`.

 Where a 'FileLeaf` becomes an intersection and a 'TagLeaf` is subject to normal
 distribution.

 Meant to operate over queries of the form:

 > (a){b}
-}
distributeTagExpression ::
  QueryExpression ->
  TagExpression (DTerm Pattern) ->
  QueryExpression
distributeTagExpression (QueryExpression qe) te =
  QueryExpression $ qe >>= distributeUnderQueryLeaf te
 where
  distributeUnderQueryLeaf ::
    TagExpression (DTerm Pattern) ->
    QueryLeaf ->
    RingExpression QueryLeaf
  distributeUnderQueryLeaf te' ql = case ql of
    FileLeaf _ -> Ring ql *. (Ring . TagLeaf $ te')
    TagLeaf te'' -> Ring . TagLeaf $ te'' ∙ te'

infixl 6 <-#

{- |
 Infix synonym for 'distributeTagExpression`.
-}
(<-#) :: QueryExpression -> TagExpression (DTerm Pattern) -> QueryExpression
(<-#) = distributeTagExpression

{- |
 Flips the operands of a binary expression.
-}
flipRingExpression :: RingExpression a -> RingExpression a
flipRingExpression r = case r of
  Ring _ -> r
  re :+ re' -> re' :+ re
  re :* re' -> re' :* re
  re :- re' -> re' :- re

{- |
 Manipulate the ring operation of the given 'TagExpression` if it is a ring operation.
-}
onTagRing ::
  TagExpression a ->
  ( forall a1.
    RingExpression a1 ->
    RingExpression a1
  ) ->
  TagExpression a
onTagRing te f = case te of
  TagRing re -> TagRing . f $ re
  _notRing -> te

{- |
 Modifies the TagExpression in a TagLeaf of a QueryExpression if it is a ring value
 and not a binary operation.
-}
onTagLeaf ::
  QueryExpression ->
  (forall a. TagExpression a -> TagExpression a) ->
  QueryExpression
onTagLeaf qe@(QueryExpression qe') f = case qe' of
  Ring ql -> case ql of
    TagLeaf te -> QueryExpression . Ring . TagLeaf . f $ te
    _notTagLeaf -> qe
  _notRingValue -> qe

onTagMagma ::
  TagExpression a ->
  ( forall a1.
    MagmaExpression a1 ->
    MagmaExpression a1
  ) ->
  TagExpression a
onTagMagma te f =
  case te of
    TagMagma m -> TagMagma . f $ m
    _notMagma -> te

{- |
 Cycles 'RingExpression` constructors for a single binary expression.
-}
nextRingOperator :: RingExpression a -> RingExpression a
nextRingOperator r = case r of
  Ring _ -> r
  re :+ re' -> re *. re'
  re :* re' -> re -. re'
  re :- re' -> re +. re'

{- |
 Drops the right operand from a 'RingExpression` if there is one.
-}
dropRightRing :: RingExpression a -> RingExpression a
dropRightRing r = case r of
  Ring _ -> r
  re :+ _ -> re
  re :* _ -> re
  re :- _ -> re

{- |
 Drops the left operand from a 'RingExpression` if there is one.
-}
dropLeftRing :: RingExpression a -> RingExpression a
dropLeftRing r =
  case r of
    Ring _ -> r
    _ :+ re -> re
    _ :* re -> re
    _ :- re -> re

{- |
 Duplicate a ring in place by intersecting it with itself.
-}
duplicateRing :: RingExpression a -> RingExpression a
duplicateRing r = r :* r

{- |
 Remove the last term from a 'MagmaExpression`
-}
cutMagmaExpression :: MagmaExpression a -> MagmaExpression a
cutMagmaExpression m = case m of
  Magma _ -> m
  me :$ _ -> me

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

findQueryExpression :: Int -> QueryExpression -> Maybe QueryExpression
findQueryExpression n =
  runIdentity
    . evalFinder
    . evaluateRing
    . fmap (mkFinder n . QueryExpression . Ring)
    . runQueryExpression

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
    . evaluateRing
    . fmap (mkEditor n f . QueryExpression . Ring)
    . runQueryExpression
    $ qe

findTagExpression :: Int -> TagExpression a -> Maybe (TagExpression a)
findTagExpression n =
  runIdentity
    . evalFinder
    . evaluateTagExpressionR (∙)
    . fmap (mkFinder n . pure)

{- |
 Modify the 'TagExpression` at the given index.
-}
withTagExpression ::
  Int ->
  TagExpression a ->
  (TagExpression a -> TagExpression a) ->
  TagExpression a
withTagExpression n te f =
  runIdentity
    . evalEditor
    . evaluateTagExpressionR (∙)
    . fmap (mkEditor n f . pure)
    $ te