{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.Expression.AST
Description : The syntax tree for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.AST (
  -- * AST

  -- ** Expressions
  Expression (..),
  ExpressionLeaf (..),

  -- ** SubExpressions
  SubExpression (..),
  SubExpressionExtension (..),

  -- ** Primitives
  BinaryExpression (..),
  TagTermExtension (..),
  TagTerm (..),
  FileTerm (..),

  -- * Classes
  IdentityKind (..),

  -- * Helpers
  expressionIdentity,
  subExpressionIdentity,
) where

import Data.Functor.Identity (Identity (..))
import Data.String (IsString)
import Data.Tagger (SetOp (..))
import Data.Text (Text)

{- |
 An 'Expression` is a structure that defines a set of 'File` that is some subset of
 the set of all 'File` in the database.

 'Expression` is a higher kinded type. This additional type can be used to annotate the
 leaves of an 'Expression` tree.

 An 'Expression` is a complete TaggerQL query.
-}
data Expression t k
  = ExpressionLeaf (t ExpressionLeaf)
  | BinaryExpressionValue (t (BinaryExpression (Expression t k)))
  | ExpressionTagTermExtension (t (TagTermExtension (SubExpression k)))

deriving instance Show (Expression Identity Identity)

deriving instance Eq (Expression Identity Identity)

deriving instance Show a => Show (Expression ((,) a) Identity)

deriving instance Eq a => Eq (Expression ((,) a) Identity)

deriving instance Show a => Show (Expression Identity ((,) a))

deriving instance Eq a => Eq (Expression Identity ((,) a))

deriving instance (Show a, Show b) => Show (Expression ((,) a) ((,) b))

deriving instance (Eq a, Eq b) => Eq (Expression ((,) a) ((,) b))

{- |
 Intermediate constructor denoting the terminal evaluations of an 'Expression`
-}
data ExpressionLeaf
  = FileTermValue FileTerm
  | TagTermValue TagTerm
  deriving (Show, Eq)

{- |
 Intermediate constructor for lifting a binary set operation over 'Expression` into
 an 'Expression`
-}
data BinaryExpression a = BinaryExpression a SetOp a
  deriving (Show, Eq, Functor)

{- |
 A type that can be modified or extended by a 'TagTerm`
-}
data TagTermExtension a = TagTermExtension TagTerm a deriving (Show, Eq, Functor)

{- |
 A 'SubExpression` is a structure that defines a set of 'Tag` that is some
 subset of the set of all 'Tag` in the database.

 A 'SubExpression` is then used:

  * as an operand in another 'SubExpression`
  * to compute a set of 'File` in an 'Expression`
  * to tag a single 'File` with the set of 'Tag` that the 'SubExpression` represents.
-}
data SubExpression k
  = -- | A search term for a set of 'Tag` that are subtags in the current environment.
    SubTag (k TagTerm)
  | SubBinary (k (BinaryExpression (SubExpression k)))
  | -- | Extends the current 'Tag` environment through the given 'TagTerm`
    -- to define a more constrained set with the given 'SubExpression`.
    SubExpression (k (SubExpressionExtension k))

deriving instance Show (SubExpression Identity)

deriving instance Eq (SubExpression Identity)

deriving instance Show a => Show (SubExpression ((,) a))

deriving instance Eq a => Eq (SubExpression ((,) a))

{- |
 Extends the current 'SubExpression` with the given 'TagTerm`
-}
data SubExpressionExtension k = SubExpressionExtension TagTerm (SubExpression k)

deriving instance Show (SubExpressionExtension Identity)

deriving instance Eq (SubExpressionExtension Identity)

deriving instance Show a => Show (SubExpressionExtension ((,) a))

deriving instance Eq a => Eq (SubExpressionExtension ((,) a))

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

{- |
 Corresponds to a search term over a filepath.
-}
newtype FileTerm
  = FileTerm Text
  deriving (Show, Eq, Semigroup, Monoid, IsString)

{- |
 A class for second kinded types that can be mapped to an Identity
-}
class IdentityKind k where
  identityKind :: k a -> Identity a

instance IdentityKind Identity where
  identityKind :: Identity a -> Identity a
  identityKind = id

instance IdentityKind ((,) a) where
  identityKind :: (a1, a2) -> Identity a2
  identityKind (_, y) = Identity y

subExpressionIdentity :: IdentityKind k => SubExpression k -> SubExpression Identity
subExpressionIdentity se = case se of
  SubTag (identityKind -> k) -> SubTag k
  SubBinary (identityKind -> Identity (BinaryExpression lhs so rhs)) ->
    SubBinary . Identity $
      BinaryExpression (subExpressionIdentity lhs) so (subExpressionIdentity rhs)
  SubExpression (identityKind -> Identity (SubExpressionExtension tt se')) ->
    SubExpression . Identity $ SubExpressionExtension tt (subExpressionIdentity se')

expressionIdentity ::
  (IdentityKind t, IdentityKind k) =>
  Expression t k ->
  Expression Identity Identity
expressionIdentity expr = case expr of
  ExpressionLeaf (identityKind -> t) -> ExpressionLeaf t
  BinaryExpressionValue (identityKind -> (Identity (BinaryExpression lhs so rhs))) ->
    BinaryExpressionValue . Identity $
      BinaryExpression (expressionIdentity lhs) so (expressionIdentity rhs)
  ExpressionTagTermExtension (identityKind -> (Identity (TagTermExtension x y))) ->
    ExpressionTagTermExtension . Identity $
      TagTermExtension x (subExpressionIdentity y)
