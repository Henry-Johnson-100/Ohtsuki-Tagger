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
  MagmaExpression (..),
  foldMagmaExpression,
  foldMagmaExpressionL,
  partitionLeft,
  appliedTo,
  over,
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
import Data.Functor ((<&>))
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

{- |
 A data type representing an expression over a magma. A sequence of operations can
 be concatenated and composed before or after another sequence.

 All expressions can be evaluated with the '1' variant of a foldable function,
 since they are non-empty.

 This type is more efficiently folded with a right-to-left fold. Therefore, foldr'
 is the most optimal.

 It is essentially a reversed, non-empty list.
-}
data MagmaExpression a
  = Magma a
  | (MagmaExpression a) :$ a
  deriving (Show, Eq, Functor, Generic)

instance Hashable a => Hashable (MagmaExpression a)

instance IsList (MagmaExpression a) where
  type Item (MagmaExpression a) = a
  fromList :: [Item (MagmaExpression a)] -> MagmaExpression a
  fromList [] = error "Empty list in fromList :: [a] -> MagmaExpression a"
  fromList (x : xs) = F.foldl' over (return x) xs
  toList :: MagmaExpression a -> [Item (MagmaExpression a)]
  toList = foldr (:) []

instance Foldable MagmaExpression where
  foldr :: (a -> b -> b) -> b -> MagmaExpression a -> b
  foldr f acc me = case me of
    Magma a -> f a acc
    dme :$ a -> foldr f (f a acc) dme

{- |
 An alias for a fold @foldr1'@ over a 'MagmaExpression`
-}
foldMagmaExpression :: (a -> a -> a) -> MagmaExpression a -> a
foldMagmaExpression _ (Magma x) = x
foldMagmaExpression f (dme :$ x) = F.foldr' f x dme

{- |
 Lazy, left-associative fold over a 'MagmaExpression`
-}
foldMagmaExpressionL :: (a -> a -> a) -> MagmaExpression a -> a
foldMagmaExpressionL = F.foldl1

{- |
 Separates the left-most term in a 'MagmaExpression` from the
 rest of the expression.
-}
partitionLeft :: MagmaExpression a -> (a, Maybe (MagmaExpression a))
partitionLeft me = case me of
  Magma a -> (a, Nothing)
  me' :$ a -> case me' of
    Magma a' -> (a', Just . pure $ a)
    (partitionLeft -> (l, mea)) :$ a' ->
      let rm = a' `appliedTo` pure a
       in (l, Just $ maybe rm (<> rm) mea)

instance Traversable MagmaExpression where
  traverse ::
    Applicative f =>
    (a -> f b) ->
    MagmaExpression a ->
    f (MagmaExpression b)
  traverse f me = case me of
    Magma a -> Magma <$> f a
    dme :$ a -> (traverse f dme <&> (:$)) <*> f a

{- |
 Left-to-right composition of a 'MagmaExpression`
-}
instance Semigroup (MagmaExpression a) where
  (<>) ::
    MagmaExpression a ->
    MagmaExpression a ->
    MagmaExpression a
  r <> d = case r of
    Magma a -> a `appliedTo` d
    _leftDistTerm -> case d of
      Magma a -> r :$ a
      rd :$ a -> (r <> rd) :$ a

infix 9 :∙

{- |
 Generalizes a binary operation over type 'a`. Where evaluation is typically
 accomplished by using this structure's 'Foldable` instance.
-}
data FreeMagma a
  = FreeMagma a
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
  pure = FreeMagma
  (<*>) :: FreeMagma (a -> b) -> FreeMagma a -> FreeMagma b
  f <*> x = case f of
    FreeMagma fab -> fmap fab x
    fm :∙ fm' -> (fm <*> x) :∙ (fm' <*> x)

instance Monad FreeMagma where
  return :: a -> FreeMagma a
  return = FreeMagma
  (>>=) :: FreeMagma a -> (a -> FreeMagma b) -> FreeMagma b
  fm >>= f = case fm of
    FreeMagma a -> f a
    fm' :∙ fm_a -> (fm' >>= f) :∙ (fm_a >>= f)

{- |
 Non associative fold of a 'FreeMagma`
-}
foldFreeMagma1 :: (a -> a -> a) -> FreeMagma a -> a
foldFreeMagma1 f fm = case fm of
  FreeMagma a -> a
  fm' :∙ fm_a -> f (foldFreeMagma1 f fm') (foldFreeMagma1 f fm_a)

evaluateFreeMagma :: Magma a => FreeMagma a -> a
evaluateFreeMagma = foldFreeMagma1 (∙)

{- |
 Prepend a value to an expression, distributing it over
 all subsequent values.

 @
  let x = Magma 0
    in 1 \`appliedTo\` x == Magma 1 :$ 0
 @
-}
appliedTo :: a -> MagmaExpression a -> MagmaExpression a
x `appliedTo` rdx = case rdx of
  Magma a -> Magma x :$ a
  rd :$ a -> x `appliedTo` rd :$ a

{- |
 Append a value to the distribution. Where the previous bottom value is
 now distributed over the given pure value, after all previous distributions have taken
 place.

 @
  let x = Magma 0
    in x \`over\` 1 == Magma 0 :$ 1
 @
-}
over :: MagmaExpression a -> a -> MagmaExpression a
over = (:$)

instance Applicative MagmaExpression where
  pure :: a -> MagmaExpression a
  pure = return
  (<*>) ::
    MagmaExpression (a -> b) ->
    MagmaExpression a ->
    MagmaExpression b
  (<*>) = ap

instance Monad MagmaExpression where
  return :: a -> MagmaExpression a
  return = Magma
  (>>=) ::
    MagmaExpression a ->
    (a -> MagmaExpression b) ->
    MagmaExpression b
  rd >>= f = case rd of
    Magma a -> f a
    rd' :$ a -> (rd' >>= f) <> f a

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
  TagValue a -> Ring . FreeMagma $ a
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
    FreeMagma te' -> unwrapIdentities te'
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
 Evaluate a 'TagExpression` with a right-associative function over its 'MagmaExpression`.
-}
foldTagExpressionR :: Rng a => (a -> a -> a) -> TagExpression a -> a
foldTagExpressionR f = evaluateTagExpressionWithMagma (F.foldr1 f)

{- |
 Evaluate a 'TagExpression` with a left-associative function over its 'MagmaExpression`.
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

instance Magma (MagmaExpression a) where
  (∙) :: MagmaExpression a -> MagmaExpression a -> MagmaExpression a
  (∙) = (<>)

{- |
 A non-associative, distributive function.
-}
instance Magma (TagExpression a) where
  (∙) :: TagExpression a -> TagExpression a -> TagExpression a
  x ∙ y = TagMagma $ pure x ∙ pure y
