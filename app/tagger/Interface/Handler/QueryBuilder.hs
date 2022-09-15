{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Handler.QueryBuilder (
  TaggerQLInteractiveAST (..),
  TaggerQLSumAST (..),
  TaggerQLSumASTLift (..),
  TaggerQLQueryValidation (..),
) where

import Data.Text (Text)
import Text.TaggerQL.AST
import Text.TaggerQL.Engine.QueryEngine

runInteractiveAST :: TaggerQLSumAST -> IO TaggerQLInteractiveAST
runInteractiveAST s =
  case s of
    SumRequest r -> _

data TaggerQLInteractiveAST
  = TaggerQLInteractiveAST TaggerQLSumAST TaggerQLQueryValidation
  deriving (Show, Eq)

{- |
 A sum type containing each component of the TaggerQL AST
-}
data TaggerQLSumAST
  = SumRequest (Request Text)
  | SumSentenceTree (SentenceTree Text)
  | SumSentenceSet (SentenceSet Text)
  | SumSentence (Sentence Text)
  | SumTermTree (TermTree Text)
  | SumSimpleTerm (SimpleTerm Text)
  | SumComplexTerm (ComplexTerm Text)
  | SumTerm (Term Text)
  deriving (Show, Eq)

{- |
 Class defining a polymorphic constructor from any AST component to the
 'TaggerQLSumAST` sum type.
-}
class TaggerQLSumASTLift a where
  liftAST :: a Text -> TaggerQLSumAST

instance TaggerQLSumASTLift Request where
  liftAST :: Request Text -> TaggerQLSumAST
  liftAST = SumRequest

instance TaggerQLSumASTLift SentenceTree where
  liftAST :: SentenceTree Text -> TaggerQLSumAST
  liftAST = SumSentenceTree

instance TaggerQLSumASTLift SentenceSet where
  liftAST :: SentenceSet Text -> TaggerQLSumAST
  liftAST = SumSentenceSet

instance TaggerQLSumASTLift Sentence where
  liftAST :: Sentence Text -> TaggerQLSumAST
  liftAST = SumSentence

instance TaggerQLSumASTLift TermTree where
  liftAST :: TermTree Text -> TaggerQLSumAST
  liftAST = SumTermTree

instance TaggerQLSumASTLift SimpleTerm where
  liftAST :: SimpleTerm Text -> TaggerQLSumAST
  liftAST = SumSimpleTerm

instance TaggerQLSumASTLift ComplexTerm where
  liftAST :: ComplexTerm Text -> TaggerQLSumAST
  liftAST = SumComplexTerm

instance TaggerQLSumASTLift Term where
  liftAST :: Term Text -> TaggerQLSumAST
  liftAST = SumTerm

{- |
 An ordered enumeration of various checks and validations
 that should occur, used to report erroneous or redundant query portions
 to the user.

 These validations happen in the Ord order for this enumeration.
-}
data TaggerQLQueryValidation
  = TaggerQLFailedToParser
  | TaggerQLReturnsEmptyResult
  | -- various validations to prevent redundant or destructive queries to run.
    -- mostly used for information to the user rather than an actual error state.
    IntersectionWithEmptySet
  | DifferenceWithEmptySet
  | UnionWithEmptySet
  deriving (Show, Eq, Ord, Enum)