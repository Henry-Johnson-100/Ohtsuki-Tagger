{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE GADTs #-}
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
  Pattern (.., Pattern),
  patternText,
  DTerm (..),
  runDTerm,

  -- * Language Expressions
  QueryExpression (..),
  QueryLeaf (..),
  TagExpression (..),
  foldTagExpressionR,
  foldTagExpressionL,
  foldTagExpression,
  distribute,
  distributeToNonRec,
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
  evaluateFreeCompoundExpression,
  DefaultRng (..),

  -- * Classes
  Rng (..),
  Ring (..),
  Magma (..),
) where

import Control.Monad (ap, join)
import Data.Bifunctor (bimap)
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
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

{- |
 A data type representing an expression of any Ring.
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
 A recursive expression that is ultimately resolvable to a ring expression
 of magma expressions.
-}
data TagExpression a
  = TagValue a
  | TagRing (RingExpression (TagExpression a))
  | TagMagma (FreeMagma (TagExpression a))
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (TagExpression a)

instance Applicative TagExpression where
  pure :: a -> TagExpression a
  pure = return
  (<*>) :: TagExpression (a -> b) -> TagExpression a -> TagExpression b
  (<*>) = ap

instance Monad TagExpression where
  return :: a -> TagExpression a
  return = TagValue
  (>>=) :: TagExpression a -> (a -> TagExpression b) -> TagExpression b
  ye >>= f = case ye of
    TagValue a -> f a
    TagRing re -> TagRing . fmap (>>= f) $ re
    TagMagma me -> TagMagma . fmap (>>= f) $ me

{- |
 Distribute all magma expressions over ring expressions.
 This alters the structure of the 'TagExpression` and if the function used to
 evaluate the expression is not distributive, then this function will effect the
 outcome of that as well.
-}
distribute :: TagExpression a -> TagExpression a
distribute = TagRing . fmap (TagMagma . fmap pure) . distributeToNonRec

{- |
  Distribute all magma expressions through a non-recursive intermediate structure.
-}
distributeToNonRec :: TagExpression a -> RingExpression (FreeMagma a)
distributeToNonRec te = case te of
  TagValue a -> Ring . Magma $ a
  TagRing re -> re >>= distributeToNonRec
  TagMagma me -> fmap join . traverse distributeToNonRec $ me

{- |
 Attempts to remove some redundant monadic identities.
-}
unwrapIdentities :: TagExpression a -> TagExpression a
unwrapIdentities te = case te of
  TagValue _ -> te
  TagRing re -> case re of
    Ring te' -> unwrapIdentities te'
    re' :+ re2 -> TagRing $ fmap unwrapIdentities re' :+ fmap unwrapIdentities re2
    re' :* re2 -> TagRing $ fmap unwrapIdentities re' :* fmap unwrapIdentities re2
    re' :- re2 -> TagRing $ fmap unwrapIdentities re' :- fmap unwrapIdentities re2
  TagMagma fm -> case fm of
    Magma te' -> unwrapIdentities te'
    fm' :∙ fm2 -> TagMagma $ fmap unwrapIdentities fm' :∙ fmap unwrapIdentities fm2

{- |
 Resolve structural ambiguities and redundancies by distributing the magma expressions
 throughout and unwrapping any redundant monadic identities.

 > normalize = unwrapIdentities . distribute
-}
normalize :: TagExpression a -> TagExpression a
normalize = unwrapIdentities . distribute

{- |
 Evaluate a 'TagExpression` by traversing its structure,
 applying function @f :: a -> a -> a@
 to its magma operations.
-}
foldTagExpression :: Rng a => (a -> a -> a) -> TagExpression a -> a
foldTagExpression f = evaluateTagExpressionWithMagma (foldFreeMagma1 f)

{- |
 Evaluate a 'TagExpression` with a right-associative function over its 'FreeMagma`.
-}
foldTagExpressionR :: Rng a => (a -> a -> a) -> TagExpression a -> a
foldTagExpressionR f = evaluateTagExpressionWithMagma (F.foldr1 f)

{- |
 Evaluate a 'TagExpression` with a left-associative function over its 'FreeMagma`.
-}
foldTagExpressionL :: Rng a => (a -> a -> a) -> TagExpression a -> a
foldTagExpressionL f = evaluateTagExpressionWithMagma (F.foldl1 f)

evaluateTagExpressionWithMagma ::
  Rng b =>
  (FreeMagma b -> b) ->
  TagExpression b ->
  b
evaluateTagExpressionWithMagma mf te =
  case te of
    TagValue a -> a
    TagRing re -> evaluateRing . fmap (evaluateTagExpressionWithMagma mf) $ re
    TagMagma me -> mf . fmap (evaluateTagExpressionWithMagma mf) $ me

{- |
 Newtype wrapper for a query expression, which is a ring expression of leaves
 where each leaf is resolvable to a set of files.
-}
newtype QueryExpression = QueryExpression {runQueryExpression :: RingExpression QueryLeaf}
  deriving (Show, Eq, Rng)

{- |
 A disjunction between filepath patterns and tag expressions that are resolvable
 to an expression of file sets.
-}
data QueryLeaf
  = FileLeaf Pattern
  | TagLeaf (TagExpression (DTerm Pattern))
  deriving (Show, Eq)

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
  FreeCompoundExpression t k a ->
  (forall a1. t a1 -> h a1) ->
  FreeCompoundExpression h k a
mapT fce f = case fce of
  FreeCompoundExpression a -> FreeCompoundExpression a
  T t -> T . f . fmap (`mapT` f) $ t
  K k -> K . fmap (`mapT` f) $ k

mapK ::
  (Functor t, Functor k) =>
  FreeCompoundExpression t k a ->
  (forall a1. k a1 -> h a1) ->
  FreeCompoundExpression t h a
mapK fce f = case fce of
  FreeCompoundExpression a -> FreeCompoundExpression a
  T t -> T . fmap (`mapK` f) $ t
  K k -> K . f . fmap (`mapK` f) $ k

{- |
 Distributes the operation denoted by K over the operation denoted by T.
 This creates an intermediate representation of @t(k a)@ which is then rewrapped
 in the 'FreeCompoundExpression` type
-}
distributeK ::
  (Monad t, Monad k, Traversable k) =>
  FreeCompoundExpression t k a ->
  FreeCompoundExpression t k a
distributeK = T . fmap (K . fmap pure) . distributeK'

distributeK' ::
  (Monad t, Monad k, Traversable k) =>
  FreeCompoundExpression t k a ->
  t (k a)
distributeK' fce = case fce of
  FreeCompoundExpression a -> pure . pure $ a
  T t -> t >>= distributeK'
  K k -> fmap join . traverse distributeK' $ k

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

{- |
 Not technically a rng as the constructors are not associative.
-}
instance Rng (TagExpression a) where
  (+.) :: TagExpression a -> TagExpression a -> TagExpression a
  x +. y = TagRing $ pure x +. pure y
  (*.) :: TagExpression a -> TagExpression a -> TagExpression a
  x *. y = TagRing $ pure x *. pure y
  (-.) :: TagExpression a -> TagExpression a -> TagExpression a
  x -. y = TagRing $ pure x -. pure y

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
  -- The set of all files
  mid :: QueryExpression
  mid = QueryExpression . Ring . FileLeaf $ WildCard

  -- The set of all files minus itself, an empty set.
  aid :: QueryExpression
  aid = mid -. mid

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
 A non-associative, distributive function.
-}
instance Magma (TagExpression a) where
  (∙) :: TagExpression a -> TagExpression a -> TagExpression a
  x ∙ y = TagMagma $ pure x ∙ pure y
