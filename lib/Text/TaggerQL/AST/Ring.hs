{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Text.TaggerQL.AST.Ring () where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Data.List.NonEmpty (NonEmpty)
import Data.Tagger
import Data.Text (Text)
import qualified Data.Text as T

data Term = Term QueryCriteria Text deriving (Show, Eq)

data ComplexTerm
  = BottomTerm Term
  | Term :<- (NonEmpty ComplexTerm)
  deriving (Show, Eq)

{- |
 A sum type that encompasses different key members of the 'Term` Ring.

 Every member of a 'TermRing` corresponds to a set of 'File`s from a Tagger database.
 This sum type is used to abstract building expressions that the query engine can
 interpret as expressions of operations taking place on the Ring of Sets.

 The additive identity 'Zero`
  which corresponds to a set of 0 'File`s.

 and multiplicative identity 'U` (intersection)
  which corresponds to the set of all 'File`s in a database.

  And two members for the set of Simple terms (marked 'Mundane`)
  and Complex (marked 'Relational`)

 These variable sets correspond to Simple and Complex terms in the current AST.
-}
data TermRing
  = Zero
  | U
  | Mundane Term
  | Relational ComplexTerm
  deriving (Show, Eq)

{- |
 Constructs a 'Mundane` 'TermRing`

 Unless the 'QueryCriteria` is 'FilePatternCriteria` and the pattern is equal to \"%\""
 then it constructs 'U`

 If the pattern is ever null, then 'Zero` is constructed.
-}
liftTerm :: Term -> TermRing
liftTerm t@(Term qc p) =
  case qc of
    FilePatternCriteria ->
      case T.strip p of
        "%" -> U
        notAll -> if T.null notAll then Zero else Mundane t
    _nonQuantifiable ->
      if T.null . T.strip $ p then Zero else Mundane t

{- |
 It is generally not possible to guarantee that a ComplexTerm
 can be either 'Zero` or 'U` so it is always constructed as
 'Relational`
-}
liftComplex :: ComplexTerm -> TermRing
liftComplex = Relational

data Expression
  = Universal TermRing
  | Operand Expression
  | Operator Expression SetOp Expression
  deriving (Show, Eq)

(<+|) :: TermRing -> Expression -> Expression
x <+| expr =
  case x of
    -- Union identity
    Zero -> expr
    -- Union annihilation
    U -> Universal U
    variable -> Operator (Universal variable) Union expr

(<*|) :: TermRing -> Expression -> Expression
x <*| expr =
  case x of
    -- Intersection identity
    U -> expr
    -- Intersection annihilation
    Zero -> Universal Zero
    variable -> Operator (Universal variable) Intersect expr

(<-|) :: TermRing -> Expression -> Expression
x <-| expr =
  case x of
    -- Difference annihilation
    Zero -> Universal Zero
    variable -> Operator (Universal variable) Difference expr

{- |
 Simplifies the given expression as much as possible by propagating
 identity and annihilations through an 'Operator`
-}
normalizeExpression :: Expression -> Expression
normalizeExpression expr =
  case expr of
    Operator x so y ->
      let normalX = normalizeExpression x
          normalY = normalizeExpression y
       in case so of
            Union ->
              case normalX of
                Universal Zero -> normalY
                Universal U -> normalX
                _nonLaw ->
                  case normalY of
                    Universal Zero -> normalX
                    Universal U -> normalY
                    _nonLaw -> Operator normalX so normalY
            Intersect ->
              case normalX of
                Universal Zero -> normalX
                Universal U -> normalY
                _nonLaw ->
                  case normalY of
                    Universal Zero -> normalY
                    Universal U -> normalX
                    _nonLaw -> Operator normalX so normalY
            Difference ->
              case normalX of
                Universal Zero -> Universal Zero
                _nonAnnihilating -> Operator normalX so normalY
    Operand expr' -> normalizeExpression expr'
    _normal -> expr