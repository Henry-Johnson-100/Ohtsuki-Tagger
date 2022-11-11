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
  liftSimple,
  liftComplex,
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
  = BottomTerm Term
  | Term :<- (NonEmpty ComplexTerm)
  deriving (Show, Eq)

{- |
 A sum type that encompasses different key members of the set of 'Term` sets.

 Every member of a 'TermIdentity` corresponds to a set of 'File`s from a Tagger database.
 This sum type is used to abstract building expressions that the query engine can
 interpret as expressions of operations taking place on the Set of 'Term` sets.

 The additive identity 'Zero`
  which corresponds to a set of 0 'File`s.

 and multiplicative identity 'U` (intersection)
  which corresponds to the set of all 'File`s in a database.

  And two members for the set of Simple terms (marked 'Simple`)
  and Complex (marked 'Relational`)

 These variable sets correspond to Simple and Complex terms in the current AST.
-}
data TermIdentity
  = Zero
  | U
  | Simple Term
  | Relational ComplexTerm
  deriving (Show, Eq)

{- |
 Constructs a 'Simple` 'TermIdentity`

 Unless the 'QueryCriteria` is 'FilePatternCriteria` and the pattern is equal to \"%\""
 then it constructs 'U`

 If the pattern is ever null, then 'Zero` is constructed.
-}
liftSimple :: Term -> TermIdentity
liftSimple t@(Term qc p) =
  case qc of
    FilePatternCriteria ->
      case T.strip p of
        "%" -> U
        notAll -> if T.null notAll then Zero else Simple t
    _nonQuantifiable ->
      if T.null . T.strip $ p then Zero else Simple t

{- |
 It is generally not possible to guarantee that a ComplexTerm
 can be either 'Zero` or 'U` so it is always constructed as
 'Relational`
-}
liftComplex :: ComplexTerm -> TermIdentity
liftComplex = Relational

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
