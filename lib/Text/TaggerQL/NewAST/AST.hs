{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Text.TaggerQL.NewAST.AST (
  Term (..),
  ComplexTerm (..),
  TermIdentity (..),
  Expression (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Tagger (QueryCriteria (FilePatternCriteria), SetOp)
import Data.Text (Text)
import qualified Data.Text as T

data Term = Term
  { termCriteria :: QueryCriteria
  , termPattern :: Text
  }
  deriving (Show, Eq)

data ComplexTerm
  = Bottom Term
  | Term :<- ComplexTerm
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

data TermIdentity
  = Simple Term
  | Relational ComplexTerm
  deriving (Show, Eq)

data Expression
  = Value TermIdentity
  | Expression Expression SetOp Expression
  deriving (Show, Eq)

{-
"o%yui {cute smile} d| (happy d| (happy {maybe}) i| amused) d| halloween u| (witch u| frog)"

Keeping in mind that 'Term` set operations are left-associative,
  this is the resulting expression:

((a D| b) D| c) U| d

where
  a = Relational (o%yui :<- [cute, smile])
  b = (e D| f) I| g
    where
      e = Simple happy
      f = Relational (happy :<- [maybe])
      g = Simple amused
  c = Simple halloween
  d = h U| i
    where
      h = Simple witch
      i = Simple frog

More concretely, this would be:
Expression
  (
    Expression
      (
        Expression
          (Value (Relational (o%yui :<- [cute, smile])))
          Difference
          (
            Expression
              (
                Expression
                  (Value (Simple happy))
                  Difference
                  (Value (Relational (happy :<- [maybe])))
              )
              Intersect
              (Value (Simple amused))
          )
      )
      Difference
      (Value (Simple halloween))
  )
  Union
  (
    Expression
      (Value (Simple witch))
      Union
      (Value (Simple frog))
  )

this is weird and may be difficult to parse because of the left-associativity
I wonder how it works right now, because I'm pretty sure queries are already
left-associative and I don't have to build the AST like this unless I am misremembering.
-}
