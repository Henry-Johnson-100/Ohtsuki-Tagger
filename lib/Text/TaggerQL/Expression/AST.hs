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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.Expression.AST
Description : The syntax tree for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.AST (
  -- * Components
  Pattern (.., Pattern),
  patternText,
  DTerm (..),
  runDTerm,

  -- * Language Expressions
  TagQueryExpression,
  unwrapIdentities,
  normalize,
  RingExpression (..),
  evaluateRing,
  toFreeMagma,
  FreeMagma (..),
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

  -- * Classes
  Rng (..),
  Ring (..),
  Magma (..),
) where

import Control.Monad (ap, join, (<=<))
import Data.Bifunctor (Bifunctor (..), bimap)
import qualified Data.Foldable as F
import Data.Functor.Classes (
  Eq1 (..),
  Show1 (liftShowList, liftShowsPrec),
  showsPrec1,
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
 A data type representing an expression of any Ring.

 This type is just an edge-labeled, unrooted binary tree.

 Each node is an operand and each edge is an operation.

 The edges are differentiable as addition, multiplication, and subtraction operations
 respectively.
-}
data RingExpression a
  = Ring a
  | RingExpression a :+ RingExpression a
  | RingExpression a :* RingExpression a
  | RingExpression a :- RingExpression a
  deriving (Functor, Foldable, Traversable, Generic)

instance Show1 RingExpression where
  liftShowsPrec ::
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    Int ->
    RingExpression a ->
    ShowS
  liftShowsPrec f g n re = case re of
    Ring a -> showsUnaryWith f "Ring" n a
    re' :+ re_a -> helpShow1Bin " :+ " re' re_a
    re' :* re_a -> helpShow1Bin " :* " re' re_a
    re' :- re_a -> helpShow1Bin " :- " re' re_a
   where
    helpShow1Bin s x y =
      let mShowParen r = showParen (case r of Ring _ -> False; _ -> True)
          lhs = mShowParen x $ liftShowsPrec f g n x
          c = showString s
          rhs = mShowParen y $ liftShowsPrec f g n y
       in lhs . c . rhs

-- This is a good baseline for something like a pretty printer but it makes a poor
-- Show1 instance.
-- instance Show1 RingExpression where
--   liftShowsPrec ::
--     (Int -> a -> ShowS) ->
--     ([a] -> ShowS) ->
--     Int ->
--     RingExpression a ->
--     ShowS
--   liftShowsPrec f g n re = case re of
--     Ring a -> f n a
--     re' :+ re_a -> helpShow1Bin " :+ " re' re_a
--     re' :* re_a -> helpShow1Bin " :* " re' re_a
--     re' :- re_a -> helpShow1Bin " :- " re' re_a
--    where
--     helpShow1Bin s x y =
--       let lhs = liftShowsPrec f g n x
--           c = showString s
--           rhs =
--             showParen (case y of Ring _ -> False; _ -> True) $
--               liftShowsPrec f g n y
--        in lhs . c . rhs

instance Show a => Show (RingExpression a) where
  showsPrec :: Show a => Int -> RingExpression a -> ShowS
  showsPrec = showsPrec1

instance Eq1 RingExpression where
  liftEq :: (a -> b -> Bool) -> RingExpression a -> RingExpression b -> Bool
  liftEq eq x y = case x of
    Ring a ->
      case y of
        Ring b -> eq a b
        _ -> False
    re :+ re' ->
      case y of
        rey :+ rey' -> liftEq eq re rey && liftEq eq re' rey'
        _ -> False
    re :* re' ->
      case y of
        rey :* rey' -> liftEq eq re rey && liftEq eq re' rey'
        _ -> False
    re :- re' ->
      case y of
        rey :- rey' -> liftEq eq re rey && liftEq eq re' rey'
        _ -> False

instance Eq a => Eq (RingExpression a) where
  (==) :: Eq a => RingExpression a -> RingExpression a -> Bool
  (==) = liftEq (==)

instance Hashable a => Hashable (RingExpression a)

instance Applicative RingExpression where
  pure :: a -> RingExpression a
  pure = return
  (<*>) :: RingExpression (a -> b) -> RingExpression a -> RingExpression b
  (<*>) = ap

instance Monad RingExpression where
  return :: a -> RingExpression a
  return = Ring
  (>>=) :: RingExpression a -> (a -> RingExpression b) -> RingExpression b
  r >>= f = case r of
    Ring a -> f a
    re :+ re' -> (re >>= f) :+ (re' >>= f)
    re :* re' -> (re >>= f) :* (re' >>= f)
    re :- re' -> (re >>= f) :- (re' >>= f)

{- |
 Run the computation defined by a 'RingExpression`
-}
evaluateRing :: Rng a => RingExpression a -> a
evaluateRing r = case r of
  Ring a -> a
  re :+ re' -> evaluateRing re +. evaluateRing re'
  re :* re' -> evaluateRing re *. evaluateRing re'
  re :- re' -> evaluateRing re -. evaluateRing re'

{- |
 Transform a 'RingExpression` to a more general free expression, 'FreeMagma`.

 It should be noted that the 'Foldable` instance for a 'RingExpression` and 'FreeMagma`
 behave the same. So if the ultimate goal is to perform some kind of associative
  fold of a 'FreeMagma`, then simply folding the 'RingExpression` will suffice.

 For example:

 >(toList :: RingExpression a -> [a]) == toList . toFreeMagma
-}
toFreeMagma :: RingExpression a -> FreeMagma a
toFreeMagma re = case re of
  Ring a -> pure a
  re' :+ re_a -> toFreeMagma re' :∙ toFreeMagma re_a
  re' :* re_a -> toFreeMagma re' :∙ toFreeMagma re_a
  re' :- re_a -> toFreeMagma re' :∙ toFreeMagma re_a

infix 9 :∙

{- |
 Generalizes a binary operation over type 'a`. Where evaluation is typically
 accomplished by using this structure's 'Foldable` instance.

 This type is just an unrooted binary tree where each node is an operand and each edge
 is an operation.
-}
data FreeMagma a
  = Magma a
  | (FreeMagma a) :∙ (FreeMagma a)
  deriving (Functor, Generic, Foldable, Traversable)

instance Show1 FreeMagma where
  liftShowsPrec ::
    (Int -> a -> ShowS) ->
    ([a] -> ShowS) ->
    Int ->
    FreeMagma a ->
    ShowS
  liftShowsPrec f g n x = case x of
    Magma a -> showsUnaryWith f "Magma" n a
    fm :∙ fm' ->
      let mWithParens fm'' =
            showParen
              ( case fm'' of
                  Magma _ -> False
                  _ -> True
              )
          lhs = mWithParens fm $ liftShowsPrec f g n fm
          rhs = mWithParens fm' $ liftShowsPrec f g n fm'
          c = showString " :∙ "
       in lhs . c . rhs

instance Show a => Show (FreeMagma a) where
  showsPrec :: Show a => Int -> FreeMagma a -> ShowS
  showsPrec = showsPrec1

instance Eq1 FreeMagma where
  liftEq :: (a -> b -> Bool) -> FreeMagma a -> FreeMagma b -> Bool
  liftEq eq x y = case x of
    Magma a ->
      case y of
        Magma b -> eq a b
        _ -> False
    fm :∙ fm' ->
      case y of
        a :∙ b -> liftEq eq fm a && liftEq eq fm' b
        _ -> False

instance Eq a => Eq (FreeMagma a) where
  (==) :: Eq a => FreeMagma a -> FreeMagma a -> Bool
  (==) = liftEq (==)

instance IsList (FreeMagma a) where
  type Item (FreeMagma a) = a
  fromList :: [Item (FreeMagma a)] -> FreeMagma a
  fromList [] = error "Empty list in fromList :: [a] -> FreeMagma a"
  fromList (x : xs) = F.foldl' (\m a -> m :∙ pure a) (pure x) xs
  toList :: FreeMagma a -> [Item (FreeMagma a)]
  toList = foldr (:) []

instance Hashable a => Hashable (FreeMagma a)

-- | Not a structural semigroup
instance Semigroup (FreeMagma a) where
  (<>) :: FreeMagma a -> FreeMagma a -> FreeMagma a
  (<>) = (:∙)

instance Magma (FreeMagma a) where
  (∙) :: FreeMagma a -> FreeMagma a -> FreeMagma a
  (∙) = (:∙)

instance Applicative FreeMagma where
  pure :: a -> FreeMagma a
  pure = Magma
  (<*>) :: FreeMagma (a -> b) -> FreeMagma a -> FreeMagma b
  f <*> x = case f of
    Magma fab -> fmap fab x
    fm :∙ fm' -> (fm <*> x) :∙ (fm' <*> x)

instance Monad FreeMagma where
  return :: a -> FreeMagma a
  return = Magma
  (>>=) :: FreeMagma a -> (a -> FreeMagma b) -> FreeMagma b
  fm >>= f = case fm of
    Magma a -> f a
    fm' :∙ fm_a -> (fm' >>= f) :∙ (fm_a >>= f)

{- |
 Non associative fold of a 'FreeMagma`
-}
foldFreeMagma1 :: (a -> a -> a) -> FreeMagma a -> a
foldFreeMagma1 f fm = case fm of
  Magma a -> a
  fm' :∙ fm_a -> f (foldFreeMagma1 f fm') (foldFreeMagma1 f fm_a)

evaluateFreeMagma :: Magma a => FreeMagma a -> a
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
    (\re -> case re of Ring x -> Just x; _ -> Nothing)
    (\fm -> case fm of Magma x -> Just x; _ -> Nothing)

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

instance Magma (FreeCompoundExpression FreeMagma k a) where
  (∙) = fcebh T (∙) pure pure

instance Magma (FreeCompoundExpression t FreeMagma a) where
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

instance Rng (RingExpression a) where
  (+.) :: RingExpression a -> RingExpression a -> RingExpression a
  (+.) = (:+)
  (*.) :: RingExpression a -> RingExpression a -> RingExpression a
  (*.) = (:*)
  (-.) :: RingExpression a -> RingExpression a -> RingExpression a
  (-.) = (:-)

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
 > FreeCompoundExpression RingExpression FreeMagma (DTerm Pattern)
-}
type TagQueryExpression =
  FreeCompoundExpression RingExpression FreeMagma (DTerm Pattern)

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
  aid = FreeQueryExpression . Ring . Right . Left $ WildCard
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
            ((*. (Ring . Right $ tqe)) . Ring . Left)
            (Ring . Right . (∙ tqe))
        )