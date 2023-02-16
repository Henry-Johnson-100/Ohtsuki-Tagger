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
  -- * Language Expressions

  -- ** TagQueryExpression
  TagQueryExpression,
  unwrapIdentities,
  normalize,

  -- ** QueryExpression
  QueryExpression (..),
  liftSimpleQueryRing,
  unliftQueryExpression,

  -- ** Primitive Expressions
  RingExpression,
  MagmaExpression,

  -- * Structures

  -- ** FreeDisjunctMonad
  FreeDisjunctMonad (..),
  mapT,
  mapK,
  distributeT,
  distributeK,
  flipTK,
  unwrapTK,
  evaluateFreeCompoundExpression,

  -- ** LabeledFreeTree
  LabeledFreeTree (..),
  fold1WithEdge,
  fold1WithEdgeMl,
  fold1WithEdgeMr,
  evaluateRingExpression,
  evaluateMagmaExpression,

  -- * Primitive Values
  RingOperation (..),
  Pattern (.., Pattern),
  patternText,
  DTerm (..),
  runDTerm,

  -- * Classes
  Rng (..),
  Ring (..),
  Magma (..),
) where

import Control.Monad (ap, join, (<=<))
import Data.Bifoldable (Bifoldable (bifoldr))
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Bitraversable (Bitraversable (..))
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
import GHC.Generics (Generic)

instance Hashable RingOperation

{- |
 > FreeDisjunctMonad RingExpression MagmaExpression (DTerm Pattern)
-}
type TagQueryExpression =
  FreeDisjunctMonad RingExpression MagmaExpression (DTerm Pattern)

{- |
 Attempts to remove some redundant monadic identities.
-}
unwrapIdentities :: TagQueryExpression -> TagQueryExpression
unwrapIdentities =
  unwrapTK
    (\re -> case re of Node x -> Just x; _ -> Nothing)
    (\fm -> case fm of Node x -> Just x; _ -> Nothing)

{- |
 Resolve structural ambiguities and redundancies by distributing the magma expressions
 throughout and unwrapping any redundant monadic identities.

 > normalize = unwrapIdentities . distributeK
-}
normalize :: TagQueryExpression -> TagQueryExpression
normalize = unwrapIdentities . T . fmap (K . fmap pure) . distributeK

newtype QueryExpression = QueryExpression
  { runQueryExpression ::
      -- a ring expression over a set of files
      LabeledFreeTree
        RingOperation
        ( -- The disjunction between either a termination of the expression
          -- or a left distribution of a query of type tag over a query of type file.
          --
          -- Where either of these options are resolvable to a set of files.
          Either
            -- This product type represents the left-distribution of a tag typed
            -- expression over a QueryExpression.
            --
            -- Notice how the TagQueryExpression is a proper subset of a
            -- QueryExpression in both disjunct cases.
            ( QueryExpression
            , FreeDisjunctMonad
                (LabeledFreeTree RingOperation)
                (LabeledFreeTree ())
                (DTerm Pattern)
            )
            ( -- The terminal disjunction between the types of files and tags
              -- where either option is resolvable to a set of files
              -- but evaluation is not forced until later.
              Either
                Pattern
                ( FreeDisjunctMonad
                    (LabeledFreeTree RingOperation)
                    (LabeledFreeTree ())
                    (DTerm Pattern)
                )
            )
        )
  }
  deriving (Show, Eq, Rng, Generic)

newtype Foo a b
  = Foo
      ( LabeledFreeTree
          RingOperation
          ( Either
              (Foo a b, b)
              ( Either
                  a
                  b
              )
          )
      )
  deriving (Generic, Rng)

instance Show2 Foo where
  liftShowsPrec2 ::
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    (Int -> b -> ShowS) ->
    ([b] -> ShowS) ->
    Int ->
    Foo a b ->
    ShowS
  liftShowsPrec2 af axs bf bxs n (Foo o) =
    showsUnaryWith
      ( liftShowsPrec2
          showsPrec
          showList
          ( liftShowsPrec2
              ( liftShowsPrec2
                  (liftShowsPrec2 af axs bf bxs)
                  (liftShowList2 af axs bf bxs)
                  bf
                  bxs
              )
              ( liftShowList2
                  (liftShowsPrec2 af axs bf bxs)
                  (liftShowList2 af axs bf bxs)
                  bf
                  bxs
              )
              (liftShowsPrec2 af axs bf bxs)
              (liftShowList2 af axs bf bxs)
          )
          ( liftShowList2
              ( liftShowsPrec2
                  (liftShowsPrec2 af axs bf bxs)
                  (liftShowList2 af axs bf bxs)
                  bf
                  bxs
              )
              ( liftShowList2
                  (liftShowsPrec2 af axs bf bxs)
                  (liftShowList2 af axs bf bxs)
                  bf
                  bxs
              )
              (liftShowsPrec2 af axs bf bxs)
              (liftShowList2 af axs bf bxs)
          )
      )
      "Foo"
      n
      o

instance Show a => Show1 (Foo a) where
  liftShowsPrec ::
    Show a =>
    (Int -> a1 -> ShowS) ->
    ([a1] -> ShowS) ->
    Int ->
    Foo a a1 ->
    ShowS
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show b) => Show (Foo a b) where
  showsPrec :: (Show a, Show b) => Int -> Foo a b -> ShowS
  showsPrec = showsPrec2

instance Eq2 Foo where
  liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> Foo a c -> Foo b d -> Bool
  liftEq2 aeq beq (Foo x) (Foo y) =
    liftEq2 (==) (liftEq2 (liftEq2 (liftEq2 aeq beq) beq) (liftEq2 aeq beq)) x y

instance Eq a => Eq1 (Foo a) where
  liftEq :: Eq a => (a1 -> b -> Bool) -> Foo a a1 -> Foo a b -> Bool
  liftEq beq = liftEq2 (==) beq

instance (Eq a, Eq b) => Eq (Foo a b) where
  (==) :: (Eq a, Eq b) => Foo a b -> Foo a b -> Bool
  (==) = liftEq2 (==) (==)

instance Bifunctor Foo where
  first :: (a -> b) -> Foo a c -> Foo b c
  first f (Foo o) = Foo $ fmap (bimap (first (first f)) (first f)) o
  second :: (b -> c) -> Foo a b -> Foo a c
  second f (Foo o) = Foo $ fmap (bimap (bimap (second f) f) (second f)) o

instance Bifoldable Foo where
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> Foo a b -> c
  bifoldr f g acc (Foo o) =
    foldr
      ( flip
          ( bifoldr
              (flip (bifoldr (flip (bifoldr f g)) g))
              (flip (bifoldr f g))
          )
      )
      acc
      o

instance Bitraversable Foo where
  bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> Foo a b -> f (Foo c d)
  bitraverse f g (Foo o) =
    Foo
      <$> traverse (bitraverse (bitraverse (bitraverse f g) g) (bitraverse f g)) o

simplifyLeftProduct ::
  -- | How to resolve a simple term to the target expression type
  (Either a c -> LabeledFreeTree RingOperation b) ->
  -- | Describe how successive left products are folded:
  -- > (a) {b} {c} {d} -> (a) {b{c{d}}}
  (c -> c -> c) ->
  -- | The expression to simplify
  Foo a c ->
  -- | Continuation describing how type c is applied to an expression of type b
  ( c ->
    LabeledFreeTree RingOperation b ->
    LabeledFreeTree RingOperation b
  ) ->
  LabeledFreeTree RingOperation b
simplifyLeftProduct onLeaf foldrBApplication (Foo o) applyB =
  o >>= either rec' onLeaf
 where
  rec' (Foo o', b) =
    o'
      >>= either
        (rec' . second (foldrBApplication b))
        (applyB b . onLeaf)

{- |
 To make non-recursive query expressions easier to build.
-}
liftSimpleQueryRing ::
  RingExpression (Either Pattern TagQueryExpression) ->
  QueryExpression
liftSimpleQueryRing = QueryExpression . fmap Right

{- |
 Resolves the left product by binding the 'QueryExpression` ring
 to a left distribution. Expanding terms in place and yielding a non-recursive
 type.
-}
unliftQueryExpression ::
  QueryExpression ->
  RingExpression (Either Pattern TagQueryExpression)
unliftQueryExpression = either unify pure <=< runQueryExpression
 where
  unify (QueryExpression fqe, tqe) =
    fqe
      >>= either
        (unify . second (∙ tqe))
        ( either
            ((*. (Node . Right $ tqe)) . Node . Left)
            (Node . Right . (∙ tqe))
        )

{- |
 > LabeledFreeTree RingOperation
-}
type RingExpression = LabeledFreeTree RingOperation

{- |
 > LabeledFreeTree ()
-}
type MagmaExpression = LabeledFreeTree ()

{- |
 A free monad with disjunct constructors for Functors t and k. Meaning that the
 structure can be interspersed with either type throughout.

 Conceptually it is like a type @t(k(t(k(t(k(t(...(a))))))))@
 where types t or k can occur in any order any number of times, but eventually terminate
 in some type a.

 This type is used primarily to create arbitrary expressions of operations 't` and 'k`
  over type 'a`.

 For example,
 let @T a :: *@ be a functor representing an expression of addition over type 'a`.
  and @K a :: *@ be a functor representing an expression of multiplication over type 'a`.
  and 'a`, 'b`, 'c`, 'd`, 'w`, 'x`, 'y`, and 'z` are values belonging to a type 'a`

consider that T can also be represented as \"w + x\"
and K can be represented as \"y * z\"

Therefore, the type @FreeDisjunctMonad T K a@ can be represented as: \"(a + b) * (c * d)"
  or any arbitrary combination of values in types T, K, and a.
-}
data FreeDisjunctMonad t k a
  = -- | The \"Disjunct\" refers to the higher-kinded disjunction.
    PureDisjunct a
  | T (t (FreeDisjunctMonad t k a))
  | K (k (FreeDisjunctMonad t k a))
  deriving (Generic)

instance (Show1 t, Show1 k) => Show1 (FreeDisjunctMonad t k) where
  liftShowsPrec ::
    (Show1 t, Show1 k) =>
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    Int ->
    FreeDisjunctMonad t k a ->
    ShowS
  liftShowsPrec f g n x = case x of
    PureDisjunct a -> showsUnaryWith f "PureDisjunct" n a
    T t -> showsUnaryWith (liftShowsPrec (liftShowsPrec f g) (liftShowList f g)) "T" n t
    K k -> showsUnaryWith (liftShowsPrec (liftShowsPrec f g) (liftShowList f g)) "K" n k

instance (Show1 t, Show1 k, Show a) => Show (FreeDisjunctMonad t k a) where
  showsPrec ::
    (Show1 t, Show1 k, Show a) =>
    Int ->
    FreeDisjunctMonad t k a ->
    ShowS
  showsPrec = showsPrec1

instance (Eq1 t, Eq1 k) => Eq1 (FreeDisjunctMonad t k) where
  liftEq ::
    (Eq1 t, Eq1 k) =>
    (a -> b -> Bool) ->
    FreeDisjunctMonad t k a ->
    FreeDisjunctMonad t k b ->
    Bool
  liftEq eq x y = case x of
    PureDisjunct a -> case y of
      PureDisjunct b -> eq a b
      _ -> False
    T t ->
      case y of
        T tb -> liftEq (liftEq eq) t tb
        _ -> False
    K k ->
      case y of
        K kb -> liftEq (liftEq eq) k kb
        _ -> False

instance (Eq1 t, Eq1 k, Eq a) => Eq (FreeDisjunctMonad t k a) where
  (==) ::
    (Eq1 t, Eq1 k, Eq a) =>
    FreeDisjunctMonad t k a ->
    FreeDisjunctMonad t k a ->
    Bool
  (==) = liftEq (==)

instance (Functor t, Functor k) => Functor (FreeDisjunctMonad t k) where
  fmap ::
    (Functor t, Functor k) =>
    (a -> b) ->
    FreeDisjunctMonad t k a ->
    FreeDisjunctMonad t k b
  fmap f fce = case fce of
    PureDisjunct a -> PureDisjunct (f a)
    T t -> T (fmap (fmap f) t)
    K k -> K (fmap (fmap f) k)

instance (Functor t, Functor k) => Applicative (FreeDisjunctMonad t k) where
  pure :: (Functor t, Functor k) => a -> FreeDisjunctMonad t k a
  pure = PureDisjunct
  (<*>) ::
    (Functor t, Functor k) =>
    FreeDisjunctMonad t k (a -> b) ->
    FreeDisjunctMonad t k a ->
    FreeDisjunctMonad t k b
  fcef <*> x = case fcef of
    PureDisjunct fab -> fmap fab x
    T t -> T (fmap (<*> x) t)
    K k -> K (fmap (<*> x) k)

instance (Functor t, Functor k) => Monad (FreeDisjunctMonad t k) where
  return :: (Functor t, Functor k) => a -> FreeDisjunctMonad t k a
  return = pure
  (>>=) ::
    (Functor t, Functor k) =>
    FreeDisjunctMonad t k a ->
    (a -> FreeDisjunctMonad t k b) ->
    FreeDisjunctMonad t k b
  fce >>= f = case fce of
    PureDisjunct a -> f a
    T t -> T (fmap (>>= f) t)
    K k -> K (fmap (>>= f) k)

instance (Foldable t, Foldable k) => Foldable (FreeDisjunctMonad t k) where
  foldr ::
    (Foldable t, Foldable k) =>
    (a -> b -> b) ->
    b ->
    FreeDisjunctMonad t k a ->
    b
  foldr f acc fce = case fce of
    PureDisjunct a -> f a acc
    T t -> foldr (flip (foldr f)) acc t
    K k -> foldr (flip (foldr f)) acc k

instance (Traversable t, Traversable k) => Traversable (FreeDisjunctMonad t k) where
  traverse ::
    (Traversable t, Traversable k, Applicative f) =>
    (a -> f b) ->
    FreeDisjunctMonad t k a ->
    f (FreeDisjunctMonad t k b)
  traverse f fce = case fce of
    PureDisjunct a -> PureDisjunct <$> f a
    T t -> T <$> traverse (traverse f) t
    K k -> K <$> traverse (traverse f) k

instance Rng (FreeDisjunctMonad RingExpression k a) where
  (+.) = fdmbh T (+.) pure pure
  (*.) = fdmbh T (*.) pure pure
  (-.) = fdmbh T (-.) pure pure

-- This should be literally the same thing as
-- > flipTK . (+.) . flipTK
-- only it does not require an additional Functor constraint.
instance Rng (FreeDisjunctMonad t RingExpression a) where
  (+.) = fdmbh K (+.) pure pure
  (*.) = fdmbh K (*.) pure pure
  (-.) = fdmbh K (-.) pure pure

instance Magma (FreeDisjunctMonad MagmaExpression k a) where
  (∙) = fdmbh T (∙) pure pure

instance Magma (FreeDisjunctMonad t MagmaExpression a) where
  (∙) = fdmbh K (∙) pure pure

{- |
 A helper for defining a binary operation over a preconstruction.

  where

  > fcebh constructor combinator preconstrX preconstrY x y

  Applies the function @combinator@ to @preconstrX x@ and @preconstrY y@
    before application of the final function, @constructor@

 Stands for FreeDisjunctMonad-Binary-Helper since it used to defined
 (*) kinded typeclasses for this type.
-}
fdmbh ::
  (t1 -> t2) ->
  (t3 -> t4 -> t1) ->
  (t5 -> t3) ->
  (t6 -> t4) ->
  t5 ->
  t6 ->
  t2
fdmbh dc cm cx cy x y = dc $ cm (cx x) (cy y)

mapT ::
  (Functor t, Functor k) =>
  (forall a1. t a1 -> h a1) ->
  FreeDisjunctMonad t k a ->
  FreeDisjunctMonad h k a
mapT f fce = case fce of
  PureDisjunct a -> PureDisjunct a
  T t -> T . f . fmap (mapT f) $ t
  K k -> K . fmap (mapT f) $ k

mapK ::
  (Functor t, Functor k) =>
  (forall a1. k a1 -> h a1) ->
  FreeDisjunctMonad t k a ->
  FreeDisjunctMonad t h a
mapK f fce = case fce of
  PureDisjunct a -> PureDisjunct a
  T t -> T . fmap (mapK f) $ t
  K k -> K . f . fmap (mapK f) $ k

{- |
 Distributes the operation denoted by K over the operation denoted by T.
-}
distributeT ::
  (Monad t, Monad k, Traversable t) =>
  FreeDisjunctMonad t k a ->
  k (t a)
distributeT fce = case fce of
  PureDisjunct a -> pure . pure $ a
  T t -> fmap join . traverse distributeT $ t
  K k -> k >>= distributeT

{- |
 Distributes the operation denoted by K over the operation denoted by T.
-}
distributeK ::
  (Monad t, Monad k, Traversable k) =>
  FreeDisjunctMonad t k a ->
  t (k a)
distributeK fce = case fce of
  PureDisjunct a -> pure . pure $ a
  T t -> t >>= distributeK
  K k -> fmap join . traverse distributeK $ k

{- |
 Simply flips the constructors.
-}
flipTK ::
  (Functor t, Functor k) =>
  FreeDisjunctMonad t k a ->
  FreeDisjunctMonad k t a
flipTK fce = case fce of
  PureDisjunct a -> PureDisjunct a
  T t -> K . fmap flipTK $ t
  K k -> T . fmap flipTK $ k

{- |
 Apply unwrapping functions to the inner constructors of a 'FreeDisjunctMonad`

 This can be used to remove redundant layers of monadic identities for example.
-}
unwrapTK ::
  (Functor t, Functor k) =>
  (forall a1. t a1 -> Maybe a1) ->
  (forall a1. k a1 -> Maybe a1) ->
  FreeDisjunctMonad t k a ->
  FreeDisjunctMonad t k a
unwrapTK tf kf fce =
  case fce of
    PureDisjunct _ -> fce
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
  FreeDisjunctMonad f1 f2 b ->
  b
evaluateFreeCompoundExpression f g fce =
  case fce of
    PureDisjunct a -> a
    T t -> f . fmap (evaluateFreeCompoundExpression f g) $ t
    K k -> g . fmap (evaluateFreeCompoundExpression f g) $ k

{- |
 Unrooted binary tree of node type 'a`
 with edges labeled as type 'l`

 This data structure is used primarily to model any binary operation of type 'l`
 over the type 'a`

 For instance:

 - An expression for a magma on 'a` can be
      generally expressed as @LabeledFreeTree () a@
 - An expression describing ring-like operations (+), (*), and (-) is expressed as
      @LabeledFreeTree RingOperation a@

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

instance Rng (LabeledFreeTree RingOperation a) where
  x +. y = Edge x Addition y

  x *. y = Edge x Multiplication y

  x -. y = Edge x Subtraction y

instance Magma (LabeledFreeTree () a) where
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

evaluateRingExpression :: Rng a => LabeledFreeTree RingOperation a -> a
evaluateRingExpression =
  fold1WithEdge
    ( \so -> case so of
        Addition -> (+.)
        Multiplication -> (*.)
        Subtraction -> (-.)
    )

evaluateMagmaExpression :: Magma a => LabeledFreeTree () a -> a
evaluateMagmaExpression = fold1WithEdge (const (∙))

{- |
 A type detailing how set-like collections are to be combined.
-}
data RingOperation
  = Addition
  | Multiplication
  | Subtraction
  deriving (Show, Eq, Bounded, Enum, Ord, Generic)

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

instance Ring QueryExpression where
  aid :: QueryExpression
  aid = QueryExpression . Node . Right . Left $ WildCard
  mid :: QueryExpression
  mid = aid -. aid

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
