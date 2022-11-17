{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Text.TaggerQL.Expression.AST (
  TagTerm (..),
  FileTerm (..),
  TagExpression (..),
  Expression (..),
) where

import Data.String (IsString)
import Data.Tagger (SetOp)
import Data.Text (Text)

data TagTerm
  = DescriptorTerm Text
  | MetaDescriptorTerm Text
  deriving (Show, Eq)

newtype FileTerm = FileTerm Text deriving (Show, Eq, Semigroup, Monoid, IsString)

data TagExpression
  = TagValue TagTerm
  | TagDistribution TagTerm TagExpression
  | TagBinaryDistribution TagTerm TagExpression SetOp TagExpression
  deriving (Show, Eq)

data Expression
  = UntaggedConst
  | FileTermValue FileTerm
  | TagExpression TagExpression
  | Binary Expression SetOp Expression
  deriving (Show, Eq)
