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
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use const" #-}

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
  FreeMagma (..),
  evaluateFreeMagma,
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
  Show1 (liftShowsPrec),
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

infix 9 :∙

{- |
 Generalizes a binary operation over type 'a`. Where evaluation is typically
 accomplished by using this structure's 'Foldable` instance.
-}
data FreeMagma a
  = Magma a
  | (FreeMagma a) :∙ (FreeMagma a)
  deriving (Show, Eq, Functor, Generic, Foldable, Traversable)

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
