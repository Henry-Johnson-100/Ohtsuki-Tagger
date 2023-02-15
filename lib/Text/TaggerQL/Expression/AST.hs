{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.Expression.AST
Description : The syntax tree for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.AST (
  -- * Components
  SetOp (..),
  Pattern (.., Pattern),
  patternText,
  DTerm (..),
  runDTerm,

  -- * Language Expressions
  TagQueryExpression,
  unwrapIdentities,
  normalize,
  FreeTree (..),
  foldFreeMagma1,
  evaluateFreeMagma,
  FreeCompoundExpression (..),
  mapT,
  mapK,
  distributeK,
  flipTK,
  unwrapTK,
  evaluateFreeCompoundExpression,
  FreeQueryExpression (..),
  liftSimpleQueryRing,
  unliftFreeQueryExpression,
  RingExpression,
  LabeledFreeTree (..),
  fold1WithEdge,
  fold1WithEdgeMl,
  fold1WithEdgeMr,
  evaluateRingExpression,

  -- * Classes
  Rng (..),
  Ring (..),
  Magma (..),
) where

import Control.Monad (ap, join, (<=<))
import Data.Bifoldable (Bifoldable (bifoldr))
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Bitraversable (Bitraversable (..))
import qualified Data.Foldable as F
import Data.Functor.Classes (
  Eq1 (..),
  Eq2 (..),
  Show1 (liftShowList, liftShowsPrec),
  Show2 (..),
  showsPrec1,
  showsPrec2,
  showsUnaryWith,
 )
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

{- |
 A type detailing how set-like collections are to be combined.
-}
data SetOp
  = Union
  | Intersect
  | Difference
  deriving (Show, Eq, Bounded, Enum, Ord, Generic)

instance Hashable SetOp

type RingExpression = LabeledFreeTree SetOp

{- |
 Unrooted binary tree of node type 'a`
 with edges labeled as type 'l`

 This data structure is used primarily to model any binary operation of type 'l`
 over the type 'a`

 For instance:

 - An expression for a magma on 'a` can be
      generally expressed as @LabeledFreeTree () a@
 - An expression describing ring-like operations (+), (*), and (-) is expressed as
      @LabeledFreeTree SetOp a@

 Furthermore, expressions over the structure itself can be expressed by simply duplicating
 the underlying structure:

 > fmap pure x :: LabeledFreeTree b (LabeledFreeTree b a)
-}
data LabeledFreeTree l a
  = Node a
  | Edge (LabeledFreeTree l a) l (LabeledFreeTree l a)
  deriving (Functor, Foldable, Traversable, Generic)

instance (Hashable l, Hashable a) => Hashable (LabeledFreeTree l a)

instance Show2 LabeledFreeTree where
  liftShowsPrec2 ::
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    (Int -> b -> ShowS) ->
    ([b] -> ShowS) ->
    Int ->
    LabeledFreeTree a b ->
    ShowS
  liftShowsPrec2 lf lfxs af afxs n x = case x of
    Node b -> showsUnaryWith af "LabeledFreeTree" n b
    Edge elft a elft' ->
      let liftS = liftShowsPrec2 lf lfxs af afxs n
          mShowParens = showParen True
          edge = showString "Edge "
          lhs = mShowParens $ liftS elft
          cen = lf n a
          rhs = mShowParens $ liftS elft'
       in edge . lhs . showString " " . cen . showString " " . rhs

instance Show l => Show1 (LabeledFreeTree l) where
  liftShowsPrec ::
    Show l =>
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    Int ->
    LabeledFreeTree l a ->
    ShowS
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show l, Show a) => Show (LabeledFreeTree l a) where
  showsPrec :: (Show l, Show a) => Int -> LabeledFreeTree l a -> ShowS
  showsPrec = showsPrec2

instance Eq2 LabeledFreeTree where
  liftEq2 ::
    (a -> b -> Bool) ->
    (c -> d -> Bool) ->
    LabeledFreeTree a c ->
    LabeledFreeTree b d ->
    Bool
  liftEq2 eql eqa x y = case x of
    Node c ->
      case y of
        Node y' -> eqa c y'
        _ -> False
    Edge elft a elft' ->
      case y of
        Edge yl yc yr ->
          -- compare edges first to short circuit before traversing deeper.
          eql a yc
            && liftEq2 eql eqa elft yl
            && liftEq2 eql eqa elft' yr
        _ -> False

instance Eq l => Eq1 (LabeledFreeTree l) where
  liftEq ::
    Eq l =>
    (a -> b -> Bool) ->
    LabeledFreeTree l a ->
    LabeledFreeTree l b ->
    Bool
  liftEq = liftEq2 (==)

instance (Eq l, Eq a) => Eq (LabeledFreeTree l a) where
  (==) ::
    (Eq l, Eq a) =>
    LabeledFreeTree l a ->
    LabeledFreeTree l a ->
    Bool
  (==) = liftEq2 (==) (==)

instance Applicative (LabeledFreeTree l) where
  pure :: a -> LabeledFreeTree l a
  pure = Node
  (<*>) ::
    LabeledFreeTree l (a -> b) ->
    LabeledFreeTree l a ->
    LabeledFreeTree l b
  f <*> x = case f of
    Node fab -> fmap fab x
    Edge elt l elt' -> Edge (elt <*> x) l (elt' <*> x)

instance Monad (LabeledFreeTree l) where
  return :: a -> LabeledFreeTree l a
  return = pure
  (>>=) ::
    LabeledFreeTree l a ->
    (a -> LabeledFreeTree l b) ->
    LabeledFreeTree l b
  elt >>= f = case elt of
    Node a -> f a
    Edge elt' l elt_la -> Edge (elt' >>= f) l (elt_la >>= f)

instance Bifunctor LabeledFreeTree where
  second :: (b -> c) -> LabeledFreeTree a b -> LabeledFreeTree a c
  second = fmap
  first :: (a -> b) -> LabeledFreeTree a c -> LabeledFreeTree b c
  first f elt = case elt of
    Node c -> Node c
    Edge elft a elft' -> Edge (first f elft) (f a) (first f elft')

instance Bifoldable LabeledFreeTree where
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> LabeledFreeTree a b -> c
  bifoldr f g acc elft = case elft of
    Node b -> g b acc
    Edge elft' a elft_ab ->
      let rhs = bifoldr f g acc elft_ab
          cen = f a rhs
          result = bifoldr f g cen elft'
       in result

instance Bitraversable LabeledFreeTree where
  bitraverse ::
    Applicative f =>
    (a -> f c) ->
    (b -> f d) ->
    LabeledFreeTree a b ->
    f (LabeledFreeTree c d)
  bitraverse f g elft = case elft of
    Node b -> Node <$> g b
    Edge elft' a elft_ab ->
      Edge
        <$> bitraverse f g elft'
          <*> f a
          <*> bitraverse f g elft_ab

instance Rng (LabeledFreeTree SetOp a) where
  (+.) ::
    LabeledFreeTree SetOp a ->
    LabeledFreeTree SetOp a ->
    LabeledFreeTree SetOp a
  x +. y = Edge x Union y
  (*.) ::
    LabeledFreeTree SetOp a ->
    LabeledFreeTree SetOp a ->
    LabeledFreeTree SetOp a
  x *. y = Edge x Intersect y
  (-.) ::
    LabeledFreeTree SetOp a ->
    LabeledFreeTree SetOp a ->
    LabeledFreeTree SetOp a
  x -. y = Edge x Difference y

instance Magma (LabeledFreeTree () a) where
  (∙) ::
    LabeledFreeTree () a ->
    LabeledFreeTree () a ->
    LabeledFreeTree () a
  x ∙ y = Edge x () y

fold1WithEdge :: (l -> a -> a -> a) -> LabeledFreeTree l a -> a
fold1WithEdge f elft = case elft of
  Node a -> a
  Edge elft' l elft_la ->
    f
      l
      (fold1WithEdge f elft')
      (fold1WithEdge f elft_la)

{- |
 Evaluates node operands from left to right.
-}
fold1WithEdgeMl ::
  Monad f =>
  (l -> a -> a -> f a) ->
  LabeledFreeTree l a ->
  f a
fold1WithEdgeMl f elft = case elft of
  Node a -> pure a
  Edge elft' l elft_la -> do
    lhs <- fold1WithEdgeMl f elft'
    rhs <- fold1WithEdgeMl f elft_la
    f l lhs rhs

{- |
 Evaluates node operands from right to left.
-}
fold1WithEdgeMr ::
  Monad f =>
  (t1 -> t2 -> t2 -> f t2) ->
  LabeledFreeTree t1 t2 ->
  f t2
fold1WithEdgeMr f elft =
  case elft of
    Node a -> pure a
    Edge elft' l elft_la -> do
      rhs <- fold1WithEdgeMr f elft_la
      lhs <- fold1WithEdgeMr f elft'
      f l lhs rhs

evaluateRingExpression :: Rng a => LabeledFreeTree SetOp a -> a
evaluateRingExpression =
  fold1WithEdge
    ( \so -> case so of
        Union -> (+.)
        Intersect -> (*.)
        Difference -> (-.)
    )

evaluateMagmaExpression :: Magma a => LabeledFreeTree () a -> a
evaluateMagmaExpression = fold1WithEdge (const (∙))

infix 9 :∙

{- |
 Generalizes a binary operation over type 'a`. Where evaluation is typically
 accomplished by using this structure's 'Foldable` instance.

 This type is just an unrooted binary tree where each node is an operand and each edge
 is an operation.
-}
data FreeTree a
  = FreeTree a
  | (FreeTree a) :∙ (FreeTree a)
  deriving (Functor, Generic, Foldable, Traversable)

instance Show1 FreeTree where
  liftShowsPrec ::
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    Int ->
    FreeTree a ->
    ShowS
  liftShowsPrec f g n x = case x of
    FreeTree a -> showsUnaryWith f "FreeTree" n a
    fm :∙ fm' ->
      let mWithParens fm'' =
            showParen
              ( case fm'' of
                  FreeTree _ -> False
                  _ -> True
              )
          lhs = mWithParens fm $ liftShowsPrec f g n fm
          rhs = mWithParens fm' $ liftShowsPrec f g n fm'
          c = showString " :∙ "
       in lhs . c . rhs

instance Show a => Show (FreeTree a) where
  showsPrec :: Show a => Int -> FreeTree a -> ShowS
  showsPrec = showsPrec1

instance Eq1 FreeTree where
  liftEq :: (a -> b -> Bool) -> FreeTree a -> FreeTree b -> Bool
  liftEq eq x y = case x of
    FreeTree a ->
      case y of
        FreeTree b -> eq a b
        _ -> False
    fm :∙ fm' ->
      case y of
        a :∙ b -> liftEq eq fm a && liftEq eq fm' b
        _ -> False

instance Eq a => Eq (FreeTree a) where
  (==) :: Eq a => FreeTree a -> FreeTree a -> Bool
  (==) = liftEq (==)

instance IsList (FreeTree a) where
  type Item (FreeTree a) = a
  fromList :: [Item (FreeTree a)] -> FreeTree a
  fromList [] = error "Empty list in fromList :: [a] -> FreeTree a"
  fromList (x : xs) = F.foldl' (\m a -> m :∙ pure a) (pure x) xs
  toList :: FreeTree a -> [Item (FreeTree a)]
  toList = foldr (:) []

instance Hashable a => Hashable (FreeTree a)

-- | Not a structural semigroup
instance Semigroup (FreeTree a) where
  (<>) :: FreeTree a -> FreeTree a -> FreeTree a
  (<>) = (:∙)

instance Magma (FreeTree a) where
  (∙) :: FreeTree a -> FreeTree a -> FreeTree a
  (∙) = (:∙)

instance Applicative FreeTree where
  pure :: a -> FreeTree a
  pure = FreeTree
  (<*>) :: FreeTree (a -> b) -> FreeTree a -> FreeTree b
  f <*> x = case f of
    FreeTree fab -> fmap fab x
    fm :∙ fm' -> (fm <*> x) :∙ (fm' <*> x)

instance Monad FreeTree where
  return :: a -> FreeTree a
  return = FreeTree
  (>>=) :: FreeTree a -> (a -> FreeTree b) -> FreeTree b
  fm >>= f = case fm of
    FreeTree a -> f a
    fm' :∙ fm_a -> (fm' >>= f) :∙ (fm_a >>= f)

{- |
 Non associative fold of a 'FreeTree`
-}
foldFreeMagma1 :: (a -> a -> a) -> FreeTree a -> a
foldFreeMagma1 f fm = case fm of
  FreeTree a -> a
  fm' :∙ fm_a -> f (foldFreeMagma1 f fm') (foldFreeMagma1 f fm_a)

evaluateFreeMagma :: Magma a => FreeTree a -> a
evaluateFreeMagma = foldFreeMagma1 (∙)

{- |
 A string used for searching with SQL LIKE expressions. Where a wildcard is a
 non-zero length sequence of the % character.
-}
data Pattern
  = WildCard
  | PatternText Text
  deriving (Show, Eq, Generic)

instance IsString Pattern where
  fromString :: String -> Pattern
  fromString = Pattern . T.pack

instance Semigroup Pattern where
  (<>) :: Pattern -> Pattern -> Pattern
  WildCard <> WildCard = WildCard
  (patternText -> x) <> (patternText -> y) = Pattern (x <> y)

instance Monoid Pattern where
  mempty :: Pattern
  mempty = PatternText mempty

instance Hashable Pattern

pattern Pattern :: Text -> Pattern
pattern Pattern t <-
  (patternText -> t)
  where
    Pattern t = if T.all (== '%') t then WildCard else PatternText t

patternText :: Pattern -> Text
patternText WildCard = "%"
patternText (PatternText t) = t

data DTerm a
  = DTerm a
  | DMetaTerm a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (DTerm a)

instance Applicative DTerm where
  pure :: a -> DTerm a
  pure = return
  (<*>) :: DTerm (a -> b) -> DTerm a -> DTerm b
  (<*>) = ap

instance Monad DTerm where
  return :: a -> DTerm a
  return = DMetaTerm
  (>>=) :: DTerm a -> (a -> DTerm b) -> DTerm b
  d >>= f = case d of
    DTerm a ->
      let result = f a
       in case result of
            DTerm _ -> result
            DMetaTerm b -> DTerm b
    DMetaTerm a -> f a

runDTerm :: DTerm p -> p
runDTerm (DTerm x) = x
runDTerm (DMetaTerm x) = x

{- |
 Attempts to remove some redundant monadic identities.
-}
unwrapIdentities :: TagQueryExpression -> TagQueryExpression
unwrapIdentities =
  unwrapTK
    (\re -> case re of Node x -> Just x; _ -> Nothing)
    (\fm -> case fm of FreeTree x -> Just x; _ -> Nothing)

{- |
 Resolve structural ambiguities and redundancies by distributing the magma expressions
 throughout and unwrapping any redundant monadic identities.

 > normalize = unwrapIdentities . distribute
-}
normalize :: TagQueryExpression -> TagQueryExpression
normalize = unwrapIdentities . T . fmap (K . fmap pure) . distributeK

{- |
 A free data structure representing expressions of different structures of the same
 type.

 This is a free type in that the only constraint it fulfills is that its constructors
 are closed over the specified operations.
-}
data FreeCompoundExpression t k a
  = FreeCompoundExpression a
  | T (t (FreeCompoundExpression t k a))
  | K (k (FreeCompoundExpression t k a))
  deriving (Generic)

instance (Show1 t, Show1 k) => Show1 (FreeCompoundExpression t k) where
  liftShowsPrec ::
    (Show1 t, Show1 k) =>
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    Int ->
    FreeCompoundExpression t k a ->
    ShowS
  liftShowsPrec f g n x = case x of
    FreeCompoundExpression a -> showsUnaryWith f "FreeCompoundExpression" n a
    T t -> showsUnaryWith (liftShowsPrec (liftShowsPrec f g) (liftShowList f g)) "T" n t
    K k -> showsUnaryWith (liftShowsPrec (liftShowsPrec f g) (liftShowList f g)) "K" n k

instance (Show1 t, Show1 k, Show a) => Show (FreeCompoundExpression t k a) where
  showsPrec ::
    (Show1 t, Show1 k, Show a) =>
    Int ->
    FreeCompoundExpression t k a ->
    ShowS
  showsPrec = showsPrec1

instance (Eq1 t, Eq1 k) => Eq1 (FreeCompoundExpression t k) where
  liftEq ::
    (Eq1 t, Eq1 k) =>
    (a -> b -> Bool) ->
    FreeCompoundExpression t k a ->
    FreeCompoundExpression t k b ->
    Bool
  liftEq eq x y = case x of
    FreeCompoundExpression a -> case y of
      FreeCompoundExpression b -> eq a b
      _ -> False
    T t ->
      case y of
        T tb -> liftEq (liftEq eq) t tb
        _ -> False
    K k ->
      case y of
        K kb -> liftEq (liftEq eq) k kb
        _ -> False

instance (Eq1 t, Eq1 k, Eq a) => Eq (FreeCompoundExpression t k a) where
  (==) ::
    (Eq1 t, Eq1 k, Eq a) =>
    FreeCompoundExpression t k a ->
    FreeCompoundExpression t k a ->
    Bool
  (==) = liftEq (==)

instance (Functor t, Functor k) => Functor (FreeCompoundExpression t k) where
  fmap ::
    (Functor t, Functor k) =>
    (a -> b) ->
    FreeCompoundExpression t k a ->
    FreeCompoundExpression t k b
  fmap f fce = case fce of
    FreeCompoundExpression a -> FreeCompoundExpression (f a)
    T t -> T (fmap (fmap f) t)
    K k -> K (fmap (fmap f) k)

instance (Applicative t, Applicative k) => Applicative (FreeCompoundExpression t k) where
  pure :: (Applicative t, Applicative k) => a -> FreeCompoundExpression t k a
  pure = FreeCompoundExpression
  (<*>) ::
    (Applicative t, Applicative k) =>
    FreeCompoundExpression t k (a -> b) ->
    FreeCompoundExpression t k a ->
    FreeCompoundExpression t k b
  fcef <*> x = case fcef of
    FreeCompoundExpression fab -> fmap fab x
    T t -> T (fmap (<*> x) t)
    K k -> K (fmap (<*> x) k)

instance (Monad t, Monad k) => Monad (FreeCompoundExpression t k) where
  return :: (Monad t, Monad k) => a -> FreeCompoundExpression t k a
  return = pure
  (>>=) ::
    (Monad t, Monad k) =>
    FreeCompoundExpression t k a ->
    (a -> FreeCompoundExpression t k b) ->
    FreeCompoundExpression t k b
  fce >>= f = case fce of
    FreeCompoundExpression a -> f a
    T t -> T (fmap (>>= f) t)
    K k -> K (fmap (>>= f) k)

instance (Foldable t, Foldable k) => Foldable (FreeCompoundExpression t k) where
  foldr ::
    (Foldable t, Foldable k) =>
    (a -> b -> b) ->
    b ->
    FreeCompoundExpression t k a ->
    b
  foldr f acc fce = case fce of
    FreeCompoundExpression a -> f a acc
    T t -> foldr (flip (foldr f)) acc t
    K k -> foldr (flip (foldr f)) acc k

instance (Traversable t, Traversable k) => Traversable (FreeCompoundExpression t k) where
  traverse ::
    (Traversable t, Traversable k, Applicative f) =>
    (a -> f b) ->
    FreeCompoundExpression t k a ->
    f (FreeCompoundExpression t k b)
  traverse f fce = case fce of
    FreeCompoundExpression a -> FreeCompoundExpression <$> f a
    T t -> T <$> traverse (traverse f) t
    K k -> K <$> traverse (traverse f) k

instance Rng (FreeCompoundExpression RingExpression k a) where
  (+.) = fcebh T (+.) pure pure
  (*.) = fcebh T (*.) pure pure
  (-.) = fcebh T (-.) pure pure

-- This should be literally the same thing as
-- > flipTK . (+.) . flipTK
-- only it does not require an additional Functor constraint.
instance Rng (FreeCompoundExpression t RingExpression a) where
  (+.) = fcebh K (+.) pure pure
  (*.) = fcebh K (*.) pure pure
  (-.) = fcebh K (-.) pure pure

instance Magma (FreeCompoundExpression FreeTree k a) where
  (∙) = fcebh T (∙) pure pure

instance Magma (FreeCompoundExpression t FreeTree a) where
  (∙) = fcebh K (∙) pure pure

{- |
 A helper for defining a binary operation over a preconstruction.

  where

  > fcebh constructor combinator preconstrX preconstrY x y

  Applies the function @combinator@ to @preconstrX x@ and @preconstrY y@
    before application of the final function, @constructor@

 Stands for FreeCompoundExpression-Binary-Helper since it used to defined
 (*) kinded typeclasses for this type.
-}
fcebh ::
  (t1 -> t2) ->
  (t3 -> t4 -> t1) ->
  (t5 -> t3) ->
  (t6 -> t4) ->
  t5 ->
  t6 ->
  t2
fcebh dc cm cx cy x y = dc $ cm (cx x) (cy y)

mapT ::
  (Functor t, Functor k) =>
  (forall a1. t a1 -> h a1) ->
  FreeCompoundExpression t k a ->
  FreeCompoundExpression h k a
mapT f fce = case fce of
  FreeCompoundExpression a -> FreeCompoundExpression a
  T t -> T . f . fmap (mapT f) $ t
  K k -> K . fmap (mapT f) $ k

mapK ::
  (Functor t, Functor k) =>
  (forall a1. k a1 -> h a1) ->
  FreeCompoundExpression t k a ->
  FreeCompoundExpression t h a
mapK f fce = case fce of
  FreeCompoundExpression a -> FreeCompoundExpression a
  T t -> T . fmap (mapK f) $ t
  K k -> K . f . fmap (mapK f) $ k

{- |
 Distributes the operation denoted by K over the operation denoted by T.
-}
distributeK ::
  (Monad t, Monad k, Traversable k) =>
  FreeCompoundExpression t k a ->
  t (k a)
distributeK fce = case fce of
  FreeCompoundExpression a -> pure . pure $ a
  T t -> t >>= distributeK
  K k -> fmap join . traverse distributeK $ k

{- |
 Simply flips the constructors.
-}
flipTK ::
  (Functor t, Functor k) =>
  FreeCompoundExpression t k a ->
  FreeCompoundExpression k t a
flipTK fce = case fce of
  FreeCompoundExpression a -> FreeCompoundExpression a
  T t -> K . fmap flipTK $ t
  K k -> T . fmap flipTK $ k

{- |
 Apply unwrapping functions to the inner constructors of a 'FreeCompoundExpression`

 This can be used to remove redundant layers of monadic identities for example.
-}
unwrapTK ::
  (Functor t, Functor k) =>
  (forall a1. t a1 -> Maybe a1) ->
  (forall a1. k a1 -> Maybe a1) ->
  FreeCompoundExpression t k a ->
  FreeCompoundExpression t k a
unwrapTK tf kf fce =
  case fce of
    FreeCompoundExpression _ -> fce
    T t ->
      let mappedUnwrap = unwrapTK tf kf <$> t
       in fromMaybe (T mappedUnwrap) . tf $ mappedUnwrap
    K k ->
      let mappedUnwrap = unwrapTK tf kf <$> k
       in fromMaybe (K mappedUnwrap) . kf $ mappedUnwrap

evaluateFreeCompoundExpression ::
  (Functor f1, Functor f2) =>
  (f1 b -> b) ->
  (f2 b -> b) ->
  FreeCompoundExpression f1 f2 b ->
  b
evaluateFreeCompoundExpression f g fce =
  case fce of
    FreeCompoundExpression a -> a
    T t -> f . fmap (evaluateFreeCompoundExpression f g) $ t
    K k -> g . fmap (evaluateFreeCompoundExpression f g) $ k

newtype DefaultRng a = DefaultRng {runDefaultRng :: a}
  deriving
    ( Show
    , Eq
    , Semigroup
    , Monoid
    , Functor
    , Foldable
    , Traversable
    )

instance Applicative DefaultRng where
  pure :: a -> DefaultRng a
  pure = DefaultRng
  (<*>) :: DefaultRng (a -> b) -> DefaultRng a -> DefaultRng b
  (DefaultRng f) <*> (DefaultRng x) = DefaultRng (f x)

instance Monad DefaultRng where
  return :: a -> DefaultRng a
  return = pure
  (>>=) :: DefaultRng a -> (a -> DefaultRng b) -> DefaultRng b
  (DefaultRng x) >>= f = f x

infixl 7 +.
infixl 7 *.
infixl 7 -.

class Rng r where
  -- | An associative operation
  (+.) :: r -> r -> r

  -- | An associative operation
  (*.) :: r -> r -> r

  -- | The inverse of '(+.)`
  (-.) :: r -> r -> r

-- | Uses the semigroup operation for all ring operations.
instance Semigroup a => Rng (DefaultRng a) where
  (+.) :: Semigroup a => DefaultRng a -> DefaultRng a -> DefaultRng a
  (+.) = (<>)
  (*.) :: Semigroup a => DefaultRng a -> DefaultRng a -> DefaultRng a
  (*.) = (<>)
  (-.) :: Semigroup a => DefaultRng a -> DefaultRng a -> DefaultRng a
  (-.) = (<>)

instance Hashable a => Rng (HashSet a) where
  (+.) :: Hashable a => HashSet a -> HashSet a -> HashSet a
  (+.) = HS.union
  (*.) :: Hashable a => HashSet a -> HashSet a -> HashSet a
  (*.) = HS.intersection
  (-.) :: Hashable a => HashSet a -> HashSet a -> HashSet a
  (-.) = HS.difference

instance Eq a => Rng [a] where
  (+.) :: Eq a => [a] -> [a] -> [a]
  (+.) = (++)
  (*.) :: Eq a => [a] -> [a] -> [a]
  (*.) = L.intersect
  (-.) :: Eq a => [a] -> [a] -> [a]
  (-.) = (L.\\)

instance Rng Int where
  (+.) :: Int -> Int -> Int
  (+.) = (+)
  (*.) :: Int -> Int -> Int
  (*.) = (*)
  (-.) :: Int -> Int -> Int
  (-.) = (-)

instance (Rng a, Rng b) => Rng (a, b) where
  (+.) :: (Rng a, Rng b) => (a, b) -> (a, b) -> (a, b)
  x +. (a, b) = bimap (+. a) (+. b) x
  (*.) :: (Rng a, Rng b) => (a, b) -> (a, b) -> (a, b)
  x *. (a, b) = bimap (*. a) (*. b) x
  (-.) :: (Rng a, Rng b) => (a, b) -> (a, b) -> (a, b)
  x -. (a, b) = bimap (-. a) (-. b) x

class Rng r => Ring r where
  -- | Identity over '(+.)`
  aid :: r

  -- | Identity over '(*.)`
  mid :: r

instance (Ring a, Ring b) => Ring (a, b) where
  mid :: (Ring a, Ring b) => (a, b)
  mid = (mid, mid)
  aid :: (Ring a, Ring b) => (a, b)
  aid = (aid, aid)

instance Ring Int where
  aid :: Int
  aid = 0
  mid :: Int
  mid = 1

infix 9 ∙

{- |
 Class for any magma.
-}
class Magma m where
  -- | A closed binary operation.
  (∙) :: m -> m -> m

instance (Magma a, Magma b) => Magma (a, b) where
  (∙) :: (Magma a, Magma b) => (a, b) -> (a, b) -> (a, b)
  x ∙ (a, b) = bimap (∙ a) (∙ b) x

{- |
 > FreeCompoundExpression RingExpression FreeTree (DTerm Pattern)
-}
type TagQueryExpression =
  FreeCompoundExpression RingExpression FreeTree (DTerm Pattern)

newtype FreeQueryExpression = FreeQueryExpression
  { runFreeQueryExpression ::
      -- a ring expression over a set of files
      RingExpression
        ( -- The disjunction between either a termination of the expression
          -- or a left distribution of a query of type tag over a query of type file.
          --
          -- Where either of these options are resolvable to a set of files.
          Either
            -- This product type represents the left-distribution of a tag typed
            -- expression over a FreeQueryExpression.
            --
            -- Notice how the TagQueryExpression is a proper subset of a
            -- FreeQueryExpression in both disjunct cases.
            ( FreeQueryExpression
            , TagQueryExpression
            )
            ( -- The terminal disjunction between the types of files and tags
              -- where either option is resolvable to a set of files
              -- but evaluation is not forced until later.
              Either
                Pattern
                TagQueryExpression
            )
        )
  }
  deriving (Show, Eq, Rng, Generic)

instance Ring FreeQueryExpression where
  aid :: FreeQueryExpression
  aid = FreeQueryExpression . Node . Right . Left $ WildCard
  mid :: FreeQueryExpression
  mid = aid -. aid

{- |
 To make non-recursive query expressions easier to build.
-}
liftSimpleQueryRing ::
  RingExpression (Either Pattern TagQueryExpression) ->
  FreeQueryExpression
liftSimpleQueryRing = FreeQueryExpression . fmap Right

{- |
 Resolves the left product by binding the 'FreeQueryExpression` ring
 to a left distribution. Expanding terms in place and yielding a non-recursive
 type.
-}
unliftFreeQueryExpression ::
  FreeQueryExpression ->
  RingExpression (Either Pattern TagQueryExpression)
unliftFreeQueryExpression = either unify pure <=< runFreeQueryExpression
 where
  unify (FreeQueryExpression fqe, tqe) =
    fqe
      >>= either
        (unify . second (∙ tqe))
        ( either
            ((*. (Node . Right $ tqe)) . Node . Left)
            (Node . Right . (∙ tqe))
        )