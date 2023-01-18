{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  TagTerm (..),
  tagTermPatternL,
  FileTerm (..),
  BinaryOperation (..),
  lhsL,
  soL,
  rhsL,
  TagTermExtension (..),
  tagTermL,
  extensionL,
  SubExpression (..),
  Expression (..),

  -- * Components
  Pattern (..),
  pattern Pattern,
  patternText,
  DTerm (..),
  runDTerm,

  -- * Language Expressions
  QueryExpression (..),
  QueryLeaf (..),
  YuiExpression (..),
  simplifyYuiExpression,
  RingExpression (..),
  evaluateRing,
  MagmaExpression (..),
  foldMagmaExpression,
  appliedTo,
  over,
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
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.String (IsString, fromString)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsList (..))
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)

{-
 ____  _____ ____  ____  _____ ____    _  _____ _____ ____
|  _ \| ____|  _ \|  _ \| ____/ ___|  / \|_   _| ____|  _ \
| | | |  _| | |_) | |_) |  _|| |     / _ \ | | |  _| | | | |
| |_| | |___|  __/|  _ <| |__| |___ / ___ \| | | |___| |_| |
|____/|_____|_|   |_| \_\_____\____/_/   \_\_| |_____|____/
-}

{- |
 Data structure representing search terms over the set of 'Descriptor`.
-}
data TagTerm
  = -- | Corresponds to the set of `Descriptor' that matches the given 'Text`
    DescriptorTerm Text
  | -- | Corresponds to the set of 'Descriptor` that are infra to the 'Descriptor`
    -- matching the given 'Text`, including the matches themselves.
    MetaDescriptorTerm Text
  deriving (Show, Eq)

tagTermPatternL :: Lens' TagTerm Text
tagTermPatternL =
  lens
    ( \tt -> case tt of
        DescriptorTerm txt -> txt
        MetaDescriptorTerm txt -> txt
    )
    ( \tt t -> case tt of
        DescriptorTerm _ -> DescriptorTerm t
        MetaDescriptorTerm _ -> MetaDescriptorTerm t
    )

{- |
 Corresponds to a search term over a filepath.
-}
newtype FileTerm
  = FileTerm Text
  deriving (Show, Eq, Semigroup, Monoid, IsString)

{- |
 A 'SubExpression` is a structure that defines a set of 'Tag` that is some
 subset of the set of all 'Tag` in the database.

 A 'SubExpression` is then used:

  * as an operand in another 'SubExpression`
  * to compute a set of 'File` in an 'Expression`
  * to tag a single 'File` with the set of 'Tag` that the 'SubExpression` represents.
-}
data SubExpression
  = -- | A search term for a set of 'Tag` that are subtags in the current environment.
    SubTag TagTerm
  | BinarySubExpression (BinaryOperation SubExpression)
  | -- | Extends the current 'Tag` environment through the given 'TagTerm`
    -- to define a more constrained set with the given 'SubExpression`.
    SubExpression (TagTermExtension SubExpression)
  deriving (Show, Eq)

{- |
 An 'Expression` is a structure that defines a set of 'File` that is some subset of
 the set of all 'File` in the database.

 An 'Expression` is a complete TaggerQL query.
-}
data Expression
  = FileTermValue FileTerm
  | TagTermValue TagTerm
  | -- | Constructs a 'Tag` set from the given 'TagTerm`
    -- that serves as the inital environment for the given 'SubExpression`.
    --
    -- Essentially, defines the set of 'File` where 'SubExpression` are subtags
    -- of any 'Tag` appearing in the set defined by the 'TagTerm`.
    TagExpression (TagTermExtension SubExpression)
  | BinaryExpression (BinaryOperation Expression)
  deriving (Show, Eq)

data BinaryOperation a = BinaryOperation a SetOp a deriving (Show, Eq, Functor)

lhsL :: Lens' (BinaryOperation a) a
lhsL =
  lens
    (\(BinaryOperation x _ _) -> x)
    (\(BinaryOperation _ so rhs) y -> BinaryOperation y so rhs)

soL :: Lens' (BinaryOperation a) SetOp
soL =
  lens
    (\(BinaryOperation _ so _) -> so)
    (\(BinaryOperation lhs _ rhs) so -> BinaryOperation lhs so rhs)

rhsL :: Lens' (BinaryOperation a) a
rhsL =
  lens
    (\(BinaryOperation _ _ rhs) -> rhs)
    (\(BinaryOperation lhs so _) rhs -> BinaryOperation lhs so rhs)

data TagTermExtension a = TagTermExtension TagTerm a deriving (Show, Eq, Functor)

tagTermL :: Lens' (TagTermExtension a) TagTerm
tagTermL =
  lens
    (\(TagTermExtension tt _) -> tt)
    (\(TagTermExtension _ se) tt -> TagTermExtension tt se)

extensionL :: Lens' (TagTermExtension a) a
extensionL =
  lens
    (\(TagTermExtension _ se) -> se)
    (flip (<$))

{-
 _____ _   _ ____
| ____| \ | |  _ \
|  _| |  \| | | | |
| |___| |\  | |_| |
|_____|_| \_|____/

 ____  _____ ____  ____  _____ ____    _  _____ _____ ____
|  _ \| ____|  _ \|  _ \| ____/ ___|  / \|_   _| ____|  _ \
| | | |  _| | |_) | |_) |  _|| |     / _ \ | | |  _| | | | |
| |_| | |___|  __/|  _ <| |__| |___ / ___ \| | | |___| |_| |
|____/|_____|_|   |_| \_\_____\____/_/   \_\_| |_____|____/
-}

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

{- |
 A data type representing an expression of any Ring.
-}
data RingExpression a
  = Ring a
  | RingExpression a :+ RingExpression a
  | RingExpression a :^ RingExpression a
  | RingExpression a :- RingExpression a
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

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
    re :^ re' -> (re >>= f) :^ (re' >>= f)
    re :- re' -> (re >>= f) :- (re' >>= f)

{- |
 Run the computation defined by a 'RingExpression`
-}
evaluateRing :: Rng a => RingExpression a -> a
evaluateRing r = case r of
  Ring a -> a
  re :+ re' -> evaluateRing re <+> evaluateRing re'
  re :^ re' -> evaluateRing re <^> evaluateRing re'
  re :- re' -> evaluateRing re <-> evaluateRing re'

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
  = MagmaValue a
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
    MagmaValue a -> f a acc
    dme :$ a -> foldr f (f a acc) dme

{- |
 An alias for a fold @foldr1'@ over a 'MagmaExpression`
-}
foldMagmaExpression :: (a -> a -> a) -> MagmaExpression a -> a
foldMagmaExpression _ (MagmaValue x) = x
foldMagmaExpression f (dme :$ x) = F.foldr' f x dme

instance Traversable MagmaExpression where
  traverse ::
    Applicative f =>
    (a -> f b) ->
    MagmaExpression a ->
    f (MagmaExpression b)
  traverse f me = case me of
    MagmaValue a -> MagmaValue <$> f a
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
    MagmaValue a -> a `appliedTo` d
    _leftDistTerm -> case d of
      MagmaValue a -> r :$ a
      rd :$ a -> (r <> rd) :$ a

{- |
 Prepend a value to an expression, distributing it over
 all subsequent values.

 @
  let x = MagmaValue 0
    in 1 \`appliedTo\` x == MagmaValue 1 :$ 0
 @
-}
appliedTo :: a -> MagmaExpression a -> MagmaExpression a
x `appliedTo` rdx = case rdx of
  MagmaValue a -> MagmaValue x :$ a
  rd :$ a -> x `appliedTo` rd :$ a

{- |
 Append a value to the distribution. Where the previous bottom value is
 now distributed over the given pure value, after all previous distributions have taken
 place.

 @
  let x = MagmaValue 0
    in x \`over\` 1 == MagmaValue 0 :$ 1
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
  return = MagmaValue
  (>>=) ::
    MagmaExpression a ->
    (a -> MagmaExpression b) ->
    MagmaExpression b
  rd >>= f = case rd of
    MagmaValue a -> f a
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
    DTerm a -> f a >>= DTerm
    DMetaTerm a -> f a

runDTerm :: DTerm p -> p
runDTerm (DTerm x) = x
runDTerm (DMetaTerm x) = x

{- |
 A recursive expression that is ultimately resolvable to a ring expression
 of magma expressions.
-}
data YuiExpression a
  = -- | The termination of a 'YuiExpression`
    YuiExpression (RingExpression (MagmaExpression a))
  | -- | A 'RingExpression` of 'YuiExpression`
    YuiRing (RingExpression (YuiExpression a))
  | -- | A 'MagmaExpression` of 'YuiExpression`
    YuiMagma (MagmaExpression (YuiExpression a))
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Hashable a => Hashable (YuiExpression a)

instance Applicative YuiExpression where
  pure :: a -> YuiExpression a
  pure = return
  (<*>) :: YuiExpression (a -> b) -> YuiExpression a -> YuiExpression b
  (<*>) = ap

instance Monad YuiExpression where
  return :: a -> YuiExpression a
  return = YuiExpression . pure . pure
  (>>=) :: YuiExpression a -> (a -> YuiExpression b) -> YuiExpression b
  ye >>= f = case ye of
    YuiExpression re -> YuiRing $ do
      re' <- re
      pure . YuiMagma $ do
        re'' <- re'
        pure . f $ re''
    YuiRing re -> YuiRing . fmap (>>= f) $ re
    YuiMagma me -> YuiMagma . fmap (>>= f) $ me

{- |
 Distributes the magma operation associatively through the structure
 and joins ring expressions together.
-}
simplifyYuiExpression :: YuiExpression a -> RingExpression (MagmaExpression a)
simplifyYuiExpression ye = case ye of
  YuiExpression re -> re
  YuiRing re -> do
    re' <- re
    simplifyYuiExpression re'
  YuiMagma me -> fmap join . traverse simplifyYuiExpression $ me

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
  | TagLeaf (YuiExpression (DTerm Pattern))
  deriving (Show, Eq)

class Rng r where
  -- | An associative operation
  (<+>) :: r -> r -> r

  -- | An associative operation
  (<^>) :: r -> r -> r

  -- | The inverse of '(<+>)`
  (<->) :: r -> r -> r

-- | Uses the semigroup operation for all ring operations.
instance Semigroup a => Rng (DefaultRng a) where
  (<+>) :: Semigroup a => DefaultRng a -> DefaultRng a -> DefaultRng a
  (<+>) = (<>)
  (<^>) :: Semigroup a => DefaultRng a -> DefaultRng a -> DefaultRng a
  (<^>) = (<>)
  (<->) :: Semigroup a => DefaultRng a -> DefaultRng a -> DefaultRng a
  (<->) = (<>)

instance Hashable a => Rng (HashSet a) where
  (<+>) :: Hashable a => HashSet a -> HashSet a -> HashSet a
  (<+>) = HS.union
  (<^>) :: Hashable a => HashSet a -> HashSet a -> HashSet a
  (<^>) = HS.intersection
  (<->) :: Hashable a => HashSet a -> HashSet a -> HashSet a
  (<->) = HS.difference

instance Eq a => Rng [a] where
  (<+>) :: Eq a => [a] -> [a] -> [a]
  (<+>) = (++)
  (<^>) :: Eq a => [a] -> [a] -> [a]
  (<^>) = L.intersect
  (<->) :: Eq a => [a] -> [a] -> [a]
  (<->) = (L.\\)

instance Rng Int where
  (<+>) :: Int -> Int -> Int
  (<+>) = (+)
  (<^>) :: Int -> Int -> Int
  (<^>) = (*)
  (<->) :: Int -> Int -> Int
  (<->) = (-)

instance Rng SubExpression where
  (<+>) :: SubExpression -> SubExpression -> SubExpression
  x <+> y = BinarySubExpression $ BinaryOperation x Union y
  (<^>) :: SubExpression -> SubExpression -> SubExpression
  x <^> y = BinarySubExpression $ BinaryOperation x Intersect y
  (<->) :: SubExpression -> SubExpression -> SubExpression
  x <-> y = BinarySubExpression $ BinaryOperation x Difference y

instance Rng Expression where
  (<+>) :: Expression -> Expression -> Expression
  x <+> y = BinaryExpression $ BinaryOperation x Union y
  (<^>) :: Expression -> Expression -> Expression
  x <^> y = BinaryExpression $ BinaryOperation x Intersect y
  (<->) :: Expression -> Expression -> Expression
  x <-> y = BinaryExpression $ BinaryOperation x Difference y

instance Rng (RingExpression a) where
  (<+>) :: RingExpression a -> RingExpression a -> RingExpression a
  (<+>) = (:+)
  (<^>) :: RingExpression a -> RingExpression a -> RingExpression a
  (<^>) = (:^)
  (<->) :: RingExpression a -> RingExpression a -> RingExpression a
  (<->) = (:-)

{- |
 Not technically a rng as the constructors are not associative.
-}
instance Rng (YuiExpression a) where
  (<+>) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  x <+> y = YuiRing $ pure x <+> pure y
  (<^>) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  x <^> y = YuiRing $ pure x <^> pure y
  (<->) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  x <-> y = YuiRing $ pure x <-> pure y

instance (Rng a, Rng b) => Rng (a, b) where
  (<+>) :: (Rng a, Rng b) => (a, b) -> (a, b) -> (a, b)
  x <+> (a, b) = bimap (<+> a) (<+> b) x
  (<^>) :: (Rng a, Rng b) => (a, b) -> (a, b) -> (a, b)
  x <^> (a, b) = bimap (<^> a) (<^> b) x
  (<->) :: (Rng a, Rng b) => (a, b) -> (a, b) -> (a, b)
  x <-> (a, b) = bimap (<-> a) (<-> b) x

class Rng r => Ring r where
  -- | Identity over '(<+>)`
  aid :: r

  -- | Identity over '(<^>)`
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

instance Ring SubExpression where
  mid :: SubExpression
  mid = SubTag . DescriptorTerm $ "%"
  aid :: SubExpression
  aid = BinarySubExpression $ BinaryOperation mid Difference mid

instance Ring Expression where
  mid :: Expression
  mid = FileTermValue "%"
  aid :: Expression
  aid =
    let dUniverse = TagTermValue . DescriptorTerm $ "%"
     in BinaryExpression
          ( BinaryOperation
              (BinaryExpression (BinaryOperation mid Difference dUniverse))
              Intersect
              dUniverse
          )

instance Ring QueryExpression where
  -- The set of all files
  mid :: QueryExpression
  mid = QueryExpression . Ring . FileLeaf $ WildCard

  -- The set of all files minus itself, an empty set.
  aid :: QueryExpression
  aid = mid <-> mid

infix 9 @@

{- |
 Class for any magma.
-}
class Magma m where
  -- | A binary operation.
  (@@) :: m -> m -> m

instance (Magma a, Magma b) => Magma (a, b) where
  (@@) :: (Magma a, Magma b) => (a, b) -> (a, b) -> (a, b)
  x @@ (a, b) = bimap (@@ a) (@@ b) x

instance Magma (MagmaExpression a) where
  (@@) :: MagmaExpression a -> MagmaExpression a -> MagmaExpression a
  (@@) = (<>)

{- |
 A non-associative, distributive function.
-}
instance Magma (YuiExpression a) where
  (@@) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  x @@ y = YuiMagma $ pure x :$ y
