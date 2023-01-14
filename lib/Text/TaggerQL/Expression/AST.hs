{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE DeriveFoldable #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
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

  -- * Language Expressions
  RingExpression (..),
  evaluateRing,
  RightDistributiveExpression (..),
  distribute,
  over,
  evaluateRightDistributiveExpression',
  evaluateRightDistributiveExpression,

  -- * Classes
  Rng (..),
  Ring (..),
  RightDistributive (..),
) where

import Control.Monad (ap)
import Data.String (IsString, fromString)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro (Lens', lens)

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

class Rng r where
  -- | An associative operation
  (<+>) :: r -> r -> r

  -- | An associative operation
  (<^>) :: r -> r -> r

  -- | The inverse of '(<+>)`
  (<->) :: r -> r -> r

class Rng r => Ring r where
  -- | Identity over '(<+>)`
  aid :: r

  -- | Identity over '(<^>)`
  mid :: r

infixl 6 @>

{- |
 Class for structures with a right-distributive operation.
-}
class RightDistributive a where
  (@>) :: a -> a -> a

instance Rng SubExpression where
  (<+>) :: SubExpression -> SubExpression -> SubExpression
  x <+> y = BinarySubExpression $ BinaryOperation x Union y
  (<^>) :: SubExpression -> SubExpression -> SubExpression
  x <^> y = BinarySubExpression $ BinaryOperation x Intersect y
  (<->) :: SubExpression -> SubExpression -> SubExpression
  x <-> y = BinarySubExpression $ BinaryOperation x Difference y

instance Ring SubExpression where
  mid :: SubExpression
  mid = SubTag . DescriptorTerm $ "%"
  aid :: SubExpression
  aid = BinarySubExpression $ BinaryOperation mid Difference mid

instance Rng Expression where
  (<+>) :: Expression -> Expression -> Expression
  x <+> y = BinaryExpression $ BinaryOperation x Union y
  (<^>) :: Expression -> Expression -> Expression
  x <^> y = BinaryExpression $ BinaryOperation x Intersect y
  (<->) :: Expression -> Expression -> Expression
  x <-> y = BinaryExpression $ BinaryOperation x Difference y

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

{- |
 A data type representing an expression of any Ring.
-}
data RingExpression a
  = Ring a
  | RingExpression a :+ RingExpression a
  | RingExpression a :^ RingExpression a
  | RingExpression a :- RingExpression a
  deriving (Show, Eq, Functor, Foldable, Traversable)

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

instance Rng (RingExpression a) where
  (<+>) :: RingExpression a -> RingExpression a -> RingExpression a
  (<+>) = (:+)
  (<^>) :: RingExpression a -> RingExpression a -> RingExpression a
  (<^>) = (:^)
  (<->) :: RingExpression a -> RingExpression a -> RingExpression a
  (<->) = (:-)

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
 A data type representing an expression of a right-distributive magma.
-}
data RightDistributiveExpression a
  = RDistValue a
  | (RightDistributiveExpression a) :$ a
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Semigroup (RightDistributiveExpression a) where
  (<>) ::
    RightDistributiveExpression a ->
    RightDistributiveExpression a ->
    RightDistributiveExpression a
  r <> d = case r of
    RDistValue a -> distribute a d
    _leftDistTerm -> case d of
      RDistValue a -> r :$ a
      rd :$ a -> (r <> rd) :$ a

{- |
 Prepend a value to an expression, distributing it over
 all subsequent values.

 @
  let x = RDistValue 0
    in distribute 1 x == RDistValue 1 :$ 0
 @
-}
distribute :: a -> RightDistributiveExpression a -> RightDistributiveExpression a
distribute x rdx = case rdx of
  RDistValue a -> RDistValue x :$ a
  rd :$ a -> distribute x rd :$ a

{- |
 Append a value to the distribution. Where the previous bottom value is
 now distributed over the given pure value, after all previous distributions have taken
 place.

 @
  let x = RDistValue 0
    in x `over` 1 == RDistValue 0 :$ 1
 @
-}
over :: RightDistributiveExpression a -> a -> RightDistributiveExpression a
over = (:$)

{- |
  Left-associative fold over a right-distributive expression.
-}
evaluateRightDistributiveExpression' ::
  (a -> a -> a) ->
  RightDistributiveExpression a ->
  a
evaluateRightDistributiveExpression' f rd = case rd of
  RDistValue a -> a
  rd' :$ a -> evaluateRightDistributiveExpression' f rd' `f` a

evaluateRightDistributiveExpression ::
  RightDistributive a =>
  RightDistributiveExpression a ->
  a
evaluateRightDistributiveExpression = evaluateRightDistributiveExpression' (@>)

instance Applicative RightDistributiveExpression where
  pure :: a -> RightDistributiveExpression a
  pure = return
  (<*>) ::
    RightDistributiveExpression (a -> b) ->
    RightDistributiveExpression a ->
    RightDistributiveExpression b
  (<*>) = ap

instance Monad RightDistributiveExpression where
  return :: a -> RightDistributiveExpression a
  return = RDistValue
  (>>=) ::
    RightDistributiveExpression a ->
    (a -> RightDistributiveExpression b) ->
    RightDistributiveExpression b
  rd >>= f = case rd of
    RDistValue a -> f a
    rd' :$ a -> (rd' >>= f) <> f a

{- |
 A string used for searching with SQL LIKE expressions. Where a wildcard is a
 non-zero length sequence of the % character.
-}
data Pattern
  = WildCard
  | PatternText Text
  deriving (Show, Eq)

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

pattern Pattern :: Text -> Pattern
pattern Pattern t <-
  (patternText -> t)
  where
    Pattern t = if T.all (== '%') t then WildCard else PatternText t

patternText :: Pattern -> Text
patternText WildCard = "%"
patternText (PatternText t) = t