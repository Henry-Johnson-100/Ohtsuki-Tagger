{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE DeriveFoldable #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Use traverse" #-}
{-# HLINT ignore "Use join" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use section" #-}

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
  RingExpressionT (..),
  returnRingExpression,
  bindRingExpression,
  RingExpression (..),
  evaluateRing,
  MagmaExpression (..),
  foldMagmaExpression,
  appliedTo,
  over,
  RecT (..),
  returnR,
  bindR,
  (>>>=),
  liftR2,
  runRecT,
  runRec,
  DefaultRng (..),

  -- * Classes
  Rng (..),
  Ring (..),
  Magma (..),
) where

import Control.Applicative (liftA2)
import Control.Monad (ap, join, (<=<))
import Control.Monad.Trans.Class
import qualified Data.Foldable as F
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.String (IsString, fromString)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsList (..))
import Lens.Micro (Lens', lens)

-- type TagExpression = RecT (RingExpressionT MagmaExpression) (DTerm Pattern)

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

class Rng r => Ring r where
  -- | Identity over '(<+>)`
  aid :: r

  -- | Identity over '(<^>)`
  mid :: r

instance Ring Int where
  aid :: Int
  aid = 0
  mid :: Int
  mid = 1

infix 9 @@

{- |
 Class for any magma.
-}
class Magma m where
  -- | A binary operation.
  (@@) :: m -> m -> m

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

newtype RingExpressionT m a = RingExpressionT
  {runRingExpressionT :: m (RingExpression a)}
  deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (RingExpressionT Identity a)
deriving instance Eq a => Eq (RingExpressionT Identity a)
deriving instance Show a => Show (RingExpressionT MagmaExpression a)
deriving instance Eq a => Eq (RingExpressionT MagmaExpression a)

instance Monad m => Applicative (RingExpressionT m) where
  pure :: Monad m => a -> RingExpressionT m a
  pure = return
  (<*>) ::
    Monad m =>
    RingExpressionT m (a -> b) ->
    RingExpressionT m a ->
    RingExpressionT m b
  (<*>) = ap

instance Monad m => Monad (RingExpressionT m) where
  return :: Monad m => a -> RingExpressionT m a
  return = RingExpressionT . return . return
  (>>=) ::
    Monad m =>
    RingExpressionT m a ->
    (a -> RingExpressionT m b) ->
    RingExpressionT m b
  (RingExpressionT re) >>= f = RingExpressionT $ do
    re' <- re
    case re' of
      Ring a -> runRingExpressionT . f $ a
      re_a :+ re'' -> go (:+) re_a re''
      re_a :^ re'' -> go (:^) re_a re''
      re_a :- re'' -> go (:-) re_a re''
   where
    go c l r = do
      l' <- join <$> traverse (runRingExpressionT . f) l
      r' <- join <$> traverse (runRingExpressionT . f) r
      return (l' `c` r')

instance MonadTrans RingExpressionT where
  lift :: Monad m => m a -> RingExpressionT m a
  lift = RingExpressionT . fmap pure

instance Applicative m => Rng (RingExpressionT m a) where
  (<+>) ::
    Applicative m =>
    RingExpressionT m a ->
    RingExpressionT m a ->
    RingExpressionT m a
  (RingExpressionT x) <+> (RingExpressionT y) = RingExpressionT ((<+>) <$> x <*> y)
  (<^>) ::
    Applicative m =>
    RingExpressionT m a ->
    RingExpressionT m a ->
    RingExpressionT m a
  (RingExpressionT x) <^> (RingExpressionT y) = RingExpressionT ((<^>) <$> x <*> y)
  (<->) ::
    Applicative m =>
    RingExpressionT m a ->
    RingExpressionT m a ->
    RingExpressionT m a
  (RingExpressionT x) <-> (RingExpressionT y) = RingExpressionT ((<->) <$> x <*> y)

instance Magma (RingExpressionT MagmaExpression a) where
  (@@) ::
    RingExpressionT MagmaExpression a ->
    RingExpressionT MagmaExpression a ->
    RingExpressionT MagmaExpression a
  (RingExpressionT x) @@ (RingExpressionT y) = RingExpressionT (x @@ y)

returnRingExpression :: Monad m => RingExpression a -> RingExpressionT m a
returnRingExpression = RingExpressionT . return

bindRingExpression ::
  Monad m =>
  RingExpressionT m a ->
  (RingExpression a -> m (RingExpression b)) ->
  RingExpressionT m b
bindRingExpression re f =
  RingExpressionT
    . ( f
          <=< runRingExpressionT
      )
    $ re

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
  -- proposed additional constructor
  --  a :& (MagmaExpression a)
  deriving (Show, Eq, Functor)

instance Magma (MagmaExpression a) where
  (@@) :: MagmaExpression a -> MagmaExpression a -> MagmaExpression a
  (@@) = (<>)

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
    in appliedTo 1 x == MagmaValue 1 :$ 0
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
    in x `over` 1 == MagmaValue 0 :$ 1
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

data DTerm a
  = DTerm a
  | DMetaTerm a
  deriving (Show, Eq, Functor, Foldable, Traversable)

runDTerm :: DTerm p -> p
runDTerm (DTerm x) = x
runDTerm (DMetaTerm x) = x

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

{- |
 A higher order monad turning any type of kind @(* -> *)@ into a self-recursive type with
 terminal values @a@.

 It is a higher order monad in the sense that inner recursions of type @(r (RecT r a))@
 can be bound to functions @(r (RecT r a) -> RecT p b)@ to produce @RecT p b@

 Therefore, any function for \'r\' that is generic in its value like @(r a -> r a)@
 can be lifted into 'RecT` as @(r (RecT r a) -> r (RecT r a))@.
-}
data RecT r a
  = -- | The terminal leaf of a recursive tree.
    Terminal a
  | -- | A single recursion, where the structure of specific 'Recursion` is invisible to
    -- to another.
    Recursion (r (RecT r a))
  deriving (Functor, Foldable, Traversable)

type Rec a = RecT Identity a

deriving instance Show a => Show (RecT Identity a)
deriving instance Show a => Show (RecT (RingExpressionT MagmaExpression) a)
deriving instance Eq a => Eq (RecT Identity a)
deriving instance Eq a => Eq (RecT (RingExpressionT MagmaExpression) a)

instance Monad r => Applicative (RecT r) where
  pure :: Monad r => a -> RecT r a
  pure = return
  (<*>) :: Monad r => RecT r (a -> b) -> RecT r a -> RecT r b
  (<*>) = ap

instance Monad r => Monad (RecT r) where
  return :: Monad r => a -> RecT r a
  return = Terminal
  (>>=) :: Monad r => RecT r a -> (a -> RecT r b) -> RecT r b
  r >>= f = case r of
    Terminal a -> f a
    Recursion r' -> Recursion . fmap (>>= f) $ r'

instance MonadTrans RecT where
  lift :: Monad m => m a -> RecT m a
  lift = returnR

{- |
 Map the inner values of a flat structure to the termination of a recursion.
-}
terminate :: Functor r => r a -> r (RecT r a)
terminate = fmap Terminal

returnR :: Functor r => r a -> RecT r a
returnR = Recursion . terminate

bindR :: Monad r => RecT r a -> (r (RecT r a) -> RecT p b) -> RecT p b
bindR r f = case r of
  Terminal a -> f . terminate . pure $ a
  Recursion r' -> f . recurse $ r'

infixl 1 >>>=
(>>>=) :: Monad r => RecT r a -> (r (RecT r a) -> RecT p b) -> RecT p b
r >>>= f = bindR r f

{- |
 Produce another layer in recursion, turning the current structure into a single leaf.

 Where the structure of r can no longer be affected by subsequent operation.

 Note that for any @rec :: r (RecT r a)@ and a function @f :: (r a -> a)@:

 @runRecT (Recursion . id $ rec) f@ = @runRecT (Recursion . recurse $ rec) f@

 BUT

 @Recursion . id $ rec@ /= @Recursion . recurse $ rec@.
-}
recurse :: Applicative r => r (RecT r a) -> r (RecT r a)
recurse = pure . Recursion

{- |
 Given an applicative action over r,
 lift it into function operating on the current recursions.
-}
liftR2 ::
  Monad r =>
  (r (RecT r a) -> r (RecT r a) -> r (RecT r a)) ->
  RecT r a ->
  RecT r a ->
  RecT r a
liftR2 f lhs rhs =
  lhs
    >>>= ( \l ->
            rhs >>>= \r ->
              Recursion (l `f` r)
         )

runRecT :: Applicative r => RecT r a -> (r a -> a) -> a
runRecT r ret = case r of
  Terminal a -> ret . pure $ a
  Recursion r' -> ret . fmap (`runRecT` ret) $ r'

runRec :: Rec a -> a
runRec = flip runRecT runIdentity

newtype MagmaExpressionT m a = MagmaExpressionT {runMagmaExpressionT :: m (MagmaExpression a)}
  deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (MagmaExpressionT Identity a)
deriving instance Eq a => Eq (MagmaExpressionT Identity a)

instance Monad m => Applicative (MagmaExpressionT m) where
  pure :: Monad m => a -> MagmaExpressionT m a
  pure = return
  (<*>) ::
    Monad m =>
    MagmaExpressionT m (a -> b) ->
    MagmaExpressionT m a ->
    MagmaExpressionT m b
  (<*>) = ap

instance Monad m => Monad (MagmaExpressionT m) where
  return :: Monad m => a -> MagmaExpressionT m a
  return = MagmaExpressionT . pure . pure
  (>>=) ::
    Monad m =>
    MagmaExpressionT m a ->
    (a -> MagmaExpressionT m b) ->
    MagmaExpressionT m b
  (MagmaExpressionT met) >>= f = MagmaExpressionT $ do
    met' <- met
    go met'
   where
    go m =
      case m of
        MagmaValue a -> runMagmaExpressionT . f $ a
        lm :$ a -> do
          lmR <- go lm
          a' <- runMagmaExpressionT . f $ a
          return (lmR <> a')

data YuiExpression a
  = YuiValue a
  | YuiRing (RingExpression (YuiExpression a))
  | YuiMagma (MagmaExpression (YuiExpression a))
  | YuiExpression (YuiExpression a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative YuiExpression where
  pure :: a -> YuiExpression a
  pure = return
  (<*>) :: YuiExpression (a -> b) -> YuiExpression a -> YuiExpression b
  (<*>) = ap

instance Monad YuiExpression where
  return :: a -> YuiExpression a
  return = YuiValue
  (>>=) :: YuiExpression a -> (a -> YuiExpression b) -> YuiExpression b
  ye >>= f = case ye of
    YuiValue a -> f a
    YuiRing re -> YuiRing . fmap (>>= f) $ re
    YuiMagma me -> YuiMagma . fmap (>>= f) $ me
    YuiExpression ye' -> ye' >>= f

instance Rng (YuiExpression a) where
  (<+>) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  x <+> (Ring . YuiExpression -> y) = YuiRing $
    case x of
      YuiValue a -> (Ring . YuiExpression . YuiValue $ a) <+> y
      YuiRing re -> (Ring . YuiExpression . YuiRing $ re) <+> y
      YuiMagma me -> (Ring . YuiExpression . YuiMagma $ me) <+> y
      YuiExpression ye -> Ring ye <+> y
  (<^>) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  x <^> (Ring . YuiExpression -> y) = YuiRing $
    case x of
      YuiValue a -> (Ring . YuiExpression . YuiValue $ a) <^> y
      YuiRing re -> (Ring . YuiExpression . YuiRing $ re) <^> y
      YuiMagma me -> (Ring . YuiExpression . YuiMagma $ me) <^> y
      YuiExpression ye -> Ring ye <^> y
  (<->) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  x <-> (Ring . YuiExpression -> y) = YuiRing $
    case x of
      YuiValue a -> (Ring . YuiExpression . YuiValue $ a) <-> y
      YuiRing re -> (Ring . YuiExpression . YuiRing $ re) <-> y
      YuiMagma me -> (Ring . YuiExpression . YuiMagma $ me) <-> y
      YuiExpression ye -> Ring ye <-> y

instance Magma (YuiExpression a) where
  (@@) :: YuiExpression a -> YuiExpression a -> YuiExpression a
  -- Case describing how a left-hand magma expression is distributed over a normal
  -- expression on the right hand side.
  (YuiMagma x) @@ rhs = YuiMagma $ case rhs of
    -- Two magma expressions are catted together from left to right. Creating a new
    -- bottom value that the expression is distributed over.
    YuiMagma rhsme -> x <> rhsme
    -- Recursive constructor, so building a magma simply applies the magma to the inner
    -- expressions rather than its wrapper.
    YuiExpression rhsye -> x :$ rhsye
    -- Normal expressions that are appended as the bottom of the magma expression
    -- without being wrapped in a recursive YuiExpression leaf.
    _applyPureExpression -> x :$ rhs
  -- Case describing how a normal left-hand expression is added to a magma expression
  -- on the right-hand side.
  lhs @@ (YuiMagma y) = YuiMagma $ case lhs of
    -- Just as the left-hand case, two magma expressions are simple catted together.
    YuiMagma lhsme -> lhsme <> y
    -- An unwrapped recursive expression is applied to the right-hand side.
    YuiExpression lhsye -> lhsye `appliedTo` y
    -- Normal expressions are just applied to the right without any wrapping.
    _applyPureExpression -> lhs `appliedTo` y
  -- Any other case produces a brand new magma expression from two pure expressions.
  x @@ y = YuiMagma $ MagmaValue x :$ y

{- |
 Resolves the inner magma expressions using the given right-associative function.

 Essentially distributes the magma operation through each relevant expression,
 leaving the structure of the ring expressions intact.

 Use when the magma is not distributive.
-}
resolveMagmas :: YuiExpression a -> (a -> a -> a) -> RingExpression a
resolveMagmas ye f = case ye of
  YuiValue a -> pure a
  YuiRing re -> re >>= flip resolveMagmas f
  YuiMagma me ->
    foldMagmaExpression (liftA2 f)
      . fmap (flip resolveMagmas f)
      $ me
  YuiExpression ye' -> resolveMagmas ye' f

printDist' :: YuiExpression (DTerm Pattern) -> RingExpression Pattern
printDist' =
  flip
    resolveMagmas
    (\outer inner -> outer <> "{" <> inner <> "}")
    . fmap runDTerm

mk :: Pattern -> YuiExpression (DTerm Pattern)
mk = YuiValue . pure

a = mk "a"
b = mk "b"
c = mk "c"
d = mk "d"
e = mk "e"
f = mk "f"
g = mk "g"
h = mk "h"

problem :: YuiExpression (DTerm Pattern)
problem =
  ( a
      <+> ( b
              @@ (c <^> d)
          )
  )
    @@ ( (e <^> f)
          @@ (g <-> h)
       )