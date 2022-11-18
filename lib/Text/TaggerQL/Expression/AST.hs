{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Text.TaggerQL.Expression.AST (
  TagTerm (..),
  FileTerm (..),
  SubExpression (..),
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

data SubExpression
  = SubTag TagTerm
  | SubBinary SubExpression SetOp SubExpression
  | SubExpression TagTerm SubExpression
  deriving (Show, Eq)

data Expression
  = UntaggedConst
  | FileTermValue FileTerm
  | TagTermValue TagTerm
  | TagExpression TagTerm SubExpression
  | Binary Expression SetOp Expression
  deriving (Show, Eq)
