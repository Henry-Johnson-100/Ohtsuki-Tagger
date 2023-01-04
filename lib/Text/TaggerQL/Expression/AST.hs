{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.Expression.AST
Description : The syntax tree for the TaggerQL query language.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.Expression.AST (
  TagTerm (..),
  FileTerm (..),
  BinaryOperation (..),
  TagTermExtension (..),
  SubExpression (..),
  Expression (..),

  -- * Classes
  Ring (..),
) where

import Data.String (IsString)
import Data.Tagger (SetOp (..))
import Data.Text (Text)

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

data TagTermExtension a = TagTermExtension TagTerm a deriving (Show, Eq, Functor)

class Ring r where
  -- | Identity over '(<+>)`
  aid :: r

  -- | Identity over '(<^>)`
  mid :: r

  -- | An associative operation
  (<+>) :: r -> r -> r

  -- | An associative operation
  (<^>) :: r -> r -> r

  -- | The inverse of '(<+>)`
  (<->) :: r -> r -> r

instance Ring SubExpression where
  mid :: SubExpression
  mid = SubTag . DescriptorTerm $ "%"
  aid :: SubExpression
  aid = BinarySubExpression $ BinaryOperation mid Difference mid
  (<+>) :: SubExpression -> SubExpression -> SubExpression
  x <+> y = BinarySubExpression $ BinaryOperation x Union y
  (<^>) :: SubExpression -> SubExpression -> SubExpression
  x <^> y = BinarySubExpression $ BinaryOperation x Intersect y
  (<->) :: SubExpression -> SubExpression -> SubExpression
  x <-> y = BinarySubExpression $ BinaryOperation x Difference y

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
  (<+>) :: Expression -> Expression -> Expression
  x <+> y = BinaryExpression $ BinaryOperation x Union y
  (<^>) :: Expression -> Expression -> Expression
  x <^> y = BinaryExpression $ BinaryOperation x Intersect y
  (<->) :: Expression -> Expression -> Expression
  x <-> y = BinaryExpression $ BinaryOperation x Difference y