{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.NewAST.AST (
  Term (..),
  ComplexTerm (..),
  TermArity (..),
  Expression (..),
) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Tagger (QueryCriteria (..), SetOp (..))
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
data Expression t
  = Value (t TermArity)
  | Expression (Expression t) SetOp (Expression t)

instance Eq (Expression Identity) where
  (==) :: Expression Identity -> Expression Identity -> Bool
  x == y =
    case (x, y) of
      (Value x', Value y') -> runIdentity x' == runIdentity y'
      (Expression x' so' y', Expression x'' so'' y'') ->
        so' == so'' && x' == x'' && y' == y''
      _NEq -> False

instance Show (Expression Identity) where
  show :: Expression Identity -> String
  show ex =
    case ex of
      Value (runIdentity -> v) -> show v
      Expression x so y -> show x <> " `" <> show so <> "` " <> show y