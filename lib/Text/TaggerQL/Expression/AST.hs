{-# LANGUAGE DeriveFoldable #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant guard" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Use join" #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Use lambda-case" #-}
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
  TagTermExtension (..),
  tagTermL,
  extensionL,
  SubExpression (..),
  Expression (..),

  -- * Expressions
  -- $Expressions
  TExpression (..),
  evaluateTExpression,
  interpretTExpression,
  FExpression (..),
  liftFExpressionA,
  liftFExpression,
  UExpression (..),
  fromUExpression,
  evaluateUExpression,
  interpretUExpression,

  -- *Substructures
  BinaryOperation (..),
  runBinaryOperation,
  dispatchLng,
  lhsL,
  soL,
  rhsL,
  Application (..),
  runApplication,
  pattern (:$),
  rightL,
  leftL,

  -- * Text patterns
  Pattern (..),
  pattern Pattern,
  patternL,
  DTerm (..),
  runDTerm,

  -- * Interpretation Modifiers
  Distribute (..),
  distribute,
  evalDistribute,
  runDistribute,

  -- * Classes
  Lng (..),
  Ling (..),
  Endomorphism (..),
) where

import Control.Monad (ap)
import Data.Bifoldable (Bifoldable (bifoldr))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.String (IsString (fromString))
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

data BinaryOperation a = BinaryOperation a SetOp a deriving (Show, Eq, Functor, Foldable)

instance Traversable BinaryOperation where
  traverse ::
    Applicative f =>
    (a -> f b) ->
    BinaryOperation a ->
    f (BinaryOperation b)
  traverse f (BinaryOperation lhs so rhs) =
    (`BinaryOperation` so) <$> f lhs <*> f rhs

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

{- |
 Data type describing the application of type @a@ to type @b@
-}
data Application a b = Application a b deriving (Show, Eq, Functor, Foldable)

runApplication :: Application (a -> b) a -> b
runApplication (Application f x) = f x

infixr 7 :$

pattern (:$) :: a -> b -> Application a b
pattern x :$ y <-
  ((Application x y))
  where
    (:$) = Application

instance Bifunctor Application where
  first :: (a -> b) -> Application a c -> Application b c
  first f (Application x y) = Application (f x) y
  second :: (b -> c) -> Application a b -> Application a c
  second = fmap

instance Monoid a => Applicative (Application a) where
  pure :: Monoid a => a1 -> Application a a1
  pure = return
  (<*>) ::
    Monoid a =>
    Application a (a1 -> b) ->
    Application a a1 ->
    Application a b
  (<*>) = ap

instance Monoid a => Monad (Application a) where
  return :: Monoid a => a1 -> Application a a1
  return = Application mempty
  (>>=) ::
    Monoid a =>
    Application a a1 ->
    (a1 -> Application a b) ->
    Application a b
  (Application c x) >>= f = first (c <>) (f x)

rightL :: Lens' (Application a b) a
rightL =
  lens
    (\(x :$ _) -> x)
    (\r x -> first (const x) r)

leftL :: Lens' (Application a b) b
leftL =
  lens
    (\(_ :$ y) -> y)
    (\r y -> second (const y) r)

{- |
 A sum type over all text patterns with a constructor for wildcards of varying length
 sequences of the character \'%\'

 The PatternSynonym 'Pattern` can be used as a smart constructor or for pattern matching.
-}
data Pattern
  = WildCard
  | PatternText Text
  deriving (Show, Eq)

instance IsString Pattern where
  fromString :: String -> Pattern
  fromString ts = if all (== '%') ts then WildCard else PatternText (fromString ts)

pattern Pattern :: Text -> Pattern
pattern Pattern t <-
  ( \pt -> case pt of
      WildCard -> "%"
      PatternText txt -> txt ->
      t
    )
  where
    Pattern t = if T.all (== '%') t then WildCard else PatternText t

patternL :: Lens' Pattern Text
patternL = lens (\(Pattern t) -> t) (\_ t -> Pattern t)

{- |
 A type a two separate cases for evaluation:
-}
data DTerm a
  = -- | The normal case \"D\"
    DTerm a
  | -- | The meta case which is a superset of \"D\"
    DMetaTerm a
  deriving (Show, Eq, Functor)

runDTerm :: DTerm p -> p
runDTerm (DTerm x) = x
runDTerm (DMetaTerm x) = x

{- |
 An expression over the codomain of A to some domain T.
-}
data TExpression a
  = TValue a
  | BinaryTExpression (BinaryOperation (TExpression a))
  | -- | The type in a 'TExpression` can be distributively applied over a 'TExpression`
    TExpressionDistribution (Application a (TExpression a))
  deriving (Show, Eq)

instance Functor TExpression where
  fmap :: (a -> b) -> TExpression a -> TExpression b
  fmap f texpr = case texpr of
    TValue tt -> TValue (f tt)
    BinaryTExpression bo -> BinaryTExpression (fmap (fmap f) bo)
    TExpressionDistribution a -> TExpressionDistribution (bimap f (fmap f) a)

instance Applicative TExpression where
  pure :: a -> TExpression a
  pure = return
  (<*>) :: TExpression (a -> b) -> TExpression a -> TExpression b
  (<*>) = ap

instance Monad TExpression where
  return :: a -> TExpression a
  return = TValue
  (>>=) :: TExpression a -> (a -> TExpression b) -> TExpression b
  texpr >>= f = case texpr of
    TValue a -> f a
    BinaryTExpression (BinaryOperation lhs so rhs) ->
      dispatchLng so (lhs >>= f) (rhs >>= f)
    TExpressionDistribution (Application x y) -> f x @> (y >>= f)

{- |
 Folding a TExpression is 'SetOp` agnostic and largely structure agnostic
 over a given 'TExpression`:

 fix some fold function, f, for any function g and value x such that @f = foldr g x@
 and 2 TExpressions a and b, the following is true:

 @all (== (f (a |-\> b))) [c a b | c \<- [(\<+\>),(\<^\>),(\<-\>)]]@
-}
instance Foldable TExpression where
  foldr :: (a -> b -> b) -> b -> TExpression a -> b
  foldr f acc texpr = case texpr of
    TValue a -> f a acc
    BinaryTExpression (BinaryOperation lhs _ rhs) -> foldr f (foldr f acc rhs) lhs
    TExpressionDistribution (Application x y) -> f x (foldr f acc y)

instance Traversable TExpression where
  traverse :: Applicative f => (a -> f b) -> TExpression a -> f (TExpression b)
  traverse f texpr = case texpr of
    TValue a -> TValue <$> f a
    BinaryTExpression (BinaryOperation lhs so rhs) ->
      dispatchLng so <$> traverse f lhs <*> traverse f rhs
    TExpressionDistribution (Application x y) ->
      (\x' y' -> TExpressionDistribution (x' :$ y')) <$> f x <*> traverse f y

instance Lng (TExpression a) where
  (<+>) :: TExpression a -> TExpression a -> TExpression a
  x <+> y = BinaryTExpression (BinaryOperation x Union y)
  (<^>) :: TExpression a -> TExpression a -> TExpression a
  x <^> y = BinaryTExpression (BinaryOperation x Intersect y)
  (<->) :: TExpression a -> TExpression a -> TExpression a
  x <-> y = BinaryTExpression (BinaryOperation x Difference y)

instance Ling (TExpression (DTerm Pattern)) where
  mid :: TExpression (DTerm Pattern)
  mid = TValue . DTerm $ WildCard
  aid :: TExpression (DTerm Pattern)
  aid = mid <-> mid

{- |
 Distributes leaves of an expression over the given 'TExpression`.
-}
instance Endomorphism (TExpression a) where
  (@>) :: TExpression a -> TExpression a -> TExpression a
  x @> y = case x of
    TValue a -> TExpressionDistribution (a :$ y)
    BinaryTExpression bn -> BinaryTExpression $ fmap (@> y) bn
    TExpressionDistribution (Application lhs rhs) ->
      TExpressionDistribution (Application lhs (rhs @> y))

{- |
 Using the definitions provided by 'Lng` and 'Endomorphism`, operate over all
 values in a 'TExpression`.

 Note that if @Endomorphism a@ is distributive then for any constrained expression t,
 the following is true:
 > evaluateTExpression t = evalDistribute . evaluateTExpression . fmap distribute $ t

 Otherwise, if @Endomorphism a@ is not distributive, then the operation of 'TExpressionDistribution`
 can be automatically distributed over subsequent 'TExpression`s by evaluating the 'TExpression`
 after mapping it to a 'Distribute` if so desired.
-}
evaluateTExpression :: (Lng a, Endomorphism a) => TExpression a -> a
evaluateTExpression texpr = case texpr of
  TValue a -> a
  BinaryTExpression bn -> runBinaryOperation . fmap evaluateTExpression $ bn
  TExpressionDistribution (Application lhs rhs) ->
    lhs @> evaluateTExpression rhs

{- |
 A newtype wrapper for an 'Lng` and 'Endomorphism` that is *NOT* distributive.
 This newtype manually distributes a future computation over the operands of the
 'Lng` operations.

 Wrap a type in this if distribution is desired and the supplied 'Endomorphism` function
  is not innately distributive.

 Which is to say, for a function @f :: a -> a -> a@ and @(+) :: a -> a -> a@
 if @f x (y + z) /= (f x y + f x z)@ and you would like @f x@ to be distributed over
 @y + z@ then this newtype is what you're looking for.
-}
newtype Distribute a = Distribute ((a -> a) -> a)

runDistribute :: Distribute a -> (a -> a) -> a
runDistribute (Distribute x) = x

evalDistribute :: Distribute a -> a
evalDistribute x = runDistribute x id

distribute :: a -> Distribute a
distribute x = Distribute $ \ret -> ret x

distributeOverBinary ::
  (t -> t -> t) ->
  Distribute t ->
  Distribute t ->
  Distribute t
distributeOverBinary
  f
  (Distribute x)
  (Distribute y) =
    Distribute $ \ret -> x ret `f` y ret

instance Lng a => Lng (Distribute a) where
  (<+>) ::
    Lng a =>
    Distribute a ->
    Distribute a ->
    Distribute a
  (<+>) = distributeOverBinary (<+>)
  (<^>) ::
    Lng a =>
    Distribute a ->
    Distribute a ->
    Distribute a
  (<^>) = distributeOverBinary (<^>)
  (<->) ::
    Lng a =>
    Distribute a ->
    Distribute a ->
    Distribute a
  (<->) = distributeOverBinary (<->)

instance Endomorphism a => Endomorphism (Distribute a) where
  (@>) ::
    Endomorphism a =>
    Distribute a ->
    Distribute a ->
    Distribute a
  (Distribute x) @> (Distribute y) =
    Distribute $ \ret -> ret $ y (x id @>)

{- | A constrained sequence of an Applicative action over
 a 'TExpression`. The constraints define how the structure of the 'TExpression`
 effects evaluation.
-}
interpretTExpression ::
  (Applicative f, Lng b, Endomorphism b) =>
  (a -> f b) ->
  TExpression a ->
  f b
interpretTExpression f = fmap evaluateTExpression . traverse f

{- |
 An expression over the domain of F that can map a 'TExpression` from the
 domain of T.
-}
data FExpression b a
  = FValue a
  | BinaryFExpression (BinaryOperation (FExpression b a))
  | -- | 'TExpression` lifted from the domain of T to F.
    LiftTExpression (TExpression b)
  deriving (Show, Eq, Functor, Foldable)

instance Lng (FExpression b a) where
  (<+>) :: FExpression b a -> FExpression b a -> FExpression b a
  x <+> y = BinaryFExpression (BinaryOperation x Union y)
  (<^>) :: FExpression b a -> FExpression b a -> FExpression b a
  x <^> y = BinaryFExpression (BinaryOperation x Intersect y)
  (<->) :: FExpression b a -> FExpression b a -> FExpression b a
  x <-> y = BinaryFExpression (BinaryOperation x Difference y)

instance Ling (FExpression (DTerm Pattern) Pattern) where
  mid :: FExpression (DTerm Pattern) Pattern
  mid = FValue WildCard
  aid :: FExpression (DTerm Pattern) Pattern
  aid = mid <-> mid

instance Bifunctor FExpression where
  second :: (b -> c) -> FExpression a b -> FExpression a c
  second = fmap
  first :: (a -> b) -> FExpression a c -> FExpression b c
  first f fexpr = case fexpr of
    FValue c -> FValue c
    BinaryFExpression bo -> BinaryFExpression (fmap (first f) bo)
    LiftTExpression te -> LiftTExpression (fmap f te)

instance Applicative (FExpression b) where
  pure :: a -> FExpression b a
  pure = return
  (<*>) :: FExpression b1 (a -> b2) -> FExpression b1 a -> FExpression b1 b2
  (<*>) = ap

instance Monad (FExpression b) where
  return :: a -> FExpression b a
  return = FValue
  (>>=) :: FExpression b a -> (a -> FExpression b b2) -> FExpression b b2
  fexpr >>= f = case fexpr of
    FValue a -> f a
    BinaryFExpression (BinaryOperation lhs so rhs) ->
      dispatchLng so (lhs >>= f) (rhs >>= f)
    LiftTExpression te -> LiftTExpression te

instance Bifoldable FExpression where
  bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> FExpression a b -> c
  bifoldr lf rf acc fexpr = case fexpr of
    FValue b -> rf b acc
    BinaryFExpression (BinaryOperation lhs _ rhs) ->
      bifoldr lf rf (bifoldr lf rf acc rhs) lhs
    LiftTExpression te -> foldr lf acc te

instance Bitraversable FExpression where
  bitraverse ::
    Applicative f =>
    (a -> f c) ->
    (b -> f d) ->
    FExpression a b ->
    f (FExpression c d)
  bitraverse f g fexpr = case fexpr of
    FValue b -> FValue <$> g b
    BinaryFExpression bo -> BinaryFExpression <$> traverse (bitraverse f g) bo
    LiftTExpression te -> LiftTExpression <$> traverse f te

{- |
 Given an applicative morphism for the inner 'TExpression` from type @a -> f b@,

 Evaluate the structure, reducing all 'TExpression` to 'UValue` and transform all
 normal 'FExpression` constructors to 'UExpression` constructors.
-}
liftFExpressionA ::
  Applicative f =>
  (TExpression a -> f b) ->
  FExpression a b ->
  f (UExpression b)
liftFExpressionA f fexpr = case fexpr of
  FValue b -> pure . UValue $ b
  BinaryFExpression bo -> BinaryUExpression <$> traverse (liftFExpressionA f) bo
  LiftTExpression te -> UValue <$> f te

{- |
 Lift an inner 'TExpression` with a pure evaluation.
-}
liftFExpression :: (TExpression a -> b) -> FExpression a b -> UExpression b
liftFExpression f = runIdentity . liftFExpressionA (Identity . f)

{- |
 A data type representing the final expression domain U for \"Ultimate\".

 Representing an 'FExpression` after evaluating the inner type along with
 some lifting function f :: @a -> b@.

 @(Lng a, Endomorphism a) => (a -> b) -> FExpression a b -> FExpression b b@
 therefore, since each inner 'TExpression` is evaluated,
 it can be reduced to a single 'FValue`

 Therefore the type 'UExpression` is injective to 'FExpression`
 using only the constructors 'FValue` and 'BinaryFExpression`
-}
data UExpression a
  = UValue a
  | BinaryUExpression (BinaryOperation (UExpression a))
  deriving (Show, Eq, Functor, Foldable)

instance Traversable UExpression where
  traverse :: Applicative f => (a -> f b) -> UExpression a -> f (UExpression b)
  traverse f uexpr = case uexpr of
    UValue a -> UValue <$> f a
    BinaryUExpression (BinaryOperation lhs so rhs) ->
      dispatchLng so <$> traverse f lhs <*> traverse f rhs

instance Lng (UExpression a) where
  (<+>) :: UExpression a -> UExpression a -> UExpression a
  x <+> y = BinaryUExpression $ BinaryOperation x Union y
  (<^>) :: UExpression a -> UExpression a -> UExpression a
  x <^> y = BinaryUExpression $ BinaryOperation x Intersect y
  (<->) :: UExpression a -> UExpression a -> UExpression a
  x <-> y = BinaryUExpression $ BinaryOperation x Difference y

instance Applicative UExpression where
  pure :: a -> UExpression a
  pure = return
  (<*>) :: UExpression (a -> b) -> UExpression a -> UExpression b
  (<*>) = ap

instance Monad UExpression where
  return :: a -> UExpression a
  return = UValue
  (>>=) :: UExpression a -> (a -> UExpression b) -> UExpression b
  uexpr >>= f = case uexpr of
    UValue a -> f a
    BinaryUExpression (BinaryOperation lhs so rhs) ->
      dispatchLng so (lhs >>= f) (rhs >>= f)

{- |
 The injective morphism for expressions U -> f

 for all 'UEXpression` the function @liftFExpression undefined . fromUExpression@
 is valid, though unwise.
-}
fromUExpression :: UExpression a -> FExpression b a
fromUExpression uexpr = case uexpr of
  UValue a -> FValue a
  BinaryUExpression bo -> BinaryFExpression . fmap fromUExpression $ bo

{- |
 Run the structure of a 'UExpression`
-}
evaluateUExpression :: Lng a => UExpression a -> a
evaluateUExpression uexpr =
  case uexpr of
    UValue a -> a
    BinaryUExpression bn -> runBinaryOperation . fmap evaluateUExpression $ bn

{- |
 Traverse a 'UExpression` and evaluate its final structure.
-}
interpretUExpression :: (Applicative f, Lng b) => (a -> f b) -> UExpression a -> f b
interpretUExpression f = fmap evaluateUExpression . traverse f

{- $Expressions

 There are three types of Expressions in TaggerQL:

  - TExpression
  - FExpression
  - UExpression

 Each one representing a step in computation through a single domain.

 TExpressions are a subset of FExpressions which map to a different domain.
 In order to finally evaluate any given expression to the ultimate domain U, there
 first has to be an evaluation of a TExpression to its domain T.
 At the same time, an FExpression can be evaluated to its domain F.
 Finally, an additional morphism must be supplied to any given FExpression that
 transforms its inner TExpressions from T -> F so that the entire FExpression can
 be lifted from F -> U at the same time.

 The reason for this is that TExpressions rely on a right associative operation that
 the domain of an FExpression is not obligated to satisfy. Therefore, an FExpression
 must be parameterized over two distinct types and it's final evaluation is separated
 into two distinct steps to avoid the constraint @Endomorphism f => FExpression f -> f@
 that would arise from evaluation.

 Instead, the structure of an FExpression is not examined during its interpretation.
 Rather it is agnostically traversed by an applicative function. It isn't until the
 evaluation of the lifted UExpression that the structure is used.
-}

dispatchLng :: Lng a => SetOp -> a -> a -> a
dispatchLng Union = (<+>)
dispatchLng Intersect = (<^>)
dispatchLng Difference = (<->)

runBinaryOperation :: Lng a => BinaryOperation a -> a
runBinaryOperation (BinaryOperation lhs so rhs) = dispatchLng so lhs rhs

infixr 7 @>

{- |
 A class for any endomorphism.
-}
class Endomorphism r where
  -- | A binary, right-associative endomorphism.
  (@>) :: r -> r -> r

infixl 7 <+>
infixl 7 <^>
infixl 7 <->

{- |
 A typeclass for 3 left associative operations
-}
class Lng r where
  -- | A left associative operation
  (<+>) :: r -> r -> r

  -- | A left associative operation
  (<^>) :: r -> r -> r

  -- | A left associative operation
  (<->) :: r -> r -> r

{- |
 Providing identities for 'Lng` operations.

 Like a Ring but for strictly left-associative operations. Additionally, there is
 no requirement that the additive identity annihilates the multiplicative operation.

 So the set of Rings is a proper subset of 'Ling`
-}
class Lng r => Ling r where
  -- | Identity over '(<+>)`, may also annihilate '(<^>)`
  aid :: r

  -- | Identity over '(<^>)`
  mid :: r

instance Lng SubExpression where
  (<+>) :: SubExpression -> SubExpression -> SubExpression
  x <+> y = BinarySubExpression $ BinaryOperation x Union y
  (<^>) :: SubExpression -> SubExpression -> SubExpression
  x <^> y = BinarySubExpression $ BinaryOperation x Intersect y
  (<->) :: SubExpression -> SubExpression -> SubExpression
  x <-> y = BinarySubExpression $ BinaryOperation x Difference y

instance Ling SubExpression where
  mid :: SubExpression
  mid = SubTag . DescriptorTerm $ "%"
  aid :: SubExpression
  aid = BinarySubExpression $ BinaryOperation mid Difference mid

instance Lng Expression where
  (<+>) :: Expression -> Expression -> Expression
  x <+> y = BinaryExpression $ BinaryOperation x Union y
  (<^>) :: Expression -> Expression -> Expression
  x <^> y = BinaryExpression $ BinaryOperation x Intersect y
  (<->) :: Expression -> Expression -> Expression
  x <-> y = BinaryExpression $ BinaryOperation x Difference y

instance Ling Expression where
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
