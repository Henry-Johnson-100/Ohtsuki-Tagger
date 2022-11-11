{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Text.TaggerQL.NewAST.AST (
  Term (..),
  ComplexTerm (..),
  TermArity (..),
  Expression (..),
) where

import Data.Tagger (QueryCriteria, SetOp)
import Data.Text (Text)

{- |
 Smallest unit of a TaggerQL query.
-}
data Term = Term
  { termCriteria :: QueryCriteria
  , termPattern :: Text
  }
  deriving (Show, Eq)

{-
New complex term does not define these subqueries as lists, but rather singular
terms that are joined via expressions.

in this way, "a{b c d}" is actually syntactic sugar for this data type's real expression:
  "I|(a{b} i| a{c} i| a{d})"

this should allow for expressions to be more easily lifted into a complex term

we can see that complexifying an expression takes on a distributive property:
  a normal expression: "b c d" which is desugared to "b i| c i| d"
    can be distributed over by "a{}" and become "a{b c d}" which is then desugared
    to "a{b} i| a{c} i| a{d}"
-}

{- |
 Denotes a 'Term` that is subtagged by another 'Term`
-}
data ComplexTerm
  = Bottom Term
  | Term :<- ComplexTerm
  deriving (Show, Eq)

{- |
 A disjunction of 'Term` and 'ComplexTerm` based on whether a 'Term` is distributed
 over another 'Term` or not.
-}
data TermArity
  = Nullary Term
  | NAry ComplexTerm
  deriving (Show, Eq)

{- |
 An expression of a value of a term of any arity or an expression of
 a binary operation on two expressions.
-}
data Expression
  = Value TermArity
  | Expression Expression SetOp Expression
  deriving (Show, Eq)
