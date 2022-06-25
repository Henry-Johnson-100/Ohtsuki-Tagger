{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Text.TaggerQL.AST
Description : Defines the components of a TaggerQL AST.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.AST (
  Request (..),
  CombinableSentence (..),
  Sentence (..),
  TermTree (..),
  Term (..),
  combinableSentenceSetOp,
  termTreeNode,
  termTreeChildren,
  newPredicates,
) where

import qualified Data.List.NonEmpty as N
import Data.Tagger (QueryCriteria (..), SetOp (..))

{- |
 A newtype wrapper for a list of 'CombinableSentence`s.

 The 'Request` type is the final and most complete representation of a TaggerQL query.
 It essentially consists of a recursive list of queries, all represented in the
 'Request` type as the terminal parent of them all.
-}
newtype Request a = Request {request :: [CombinableSentence a]}
  deriving (Show, Eq, Functor)

{- |
 A tree of 'Sentences` and a 'SetOp` to describe how to combine them.

 All 'CombinableSentence`s terminate in a 'SimpleCombinableSentence` eventually.

 The fields are meant to be accessed primarily with pattern matching as the query engine
 must interpret the constructors differently.
-}
data CombinableSentence a
  = SimpleCombinableSentence SetOp (Sentence a)
  | ComplexCombinableSentence SetOp [CombinableSentence a]
  deriving (Show, Eq, Functor)

combinableSentenceSetOp :: CombinableSentence a -> SetOp
combinableSentenceSetOp (SimpleCombinableSentence s _) = s
combinableSentenceSetOp (ComplexCombinableSentence s _) = s

{- |
 A complete TaggerQL query.
-}
newtype Sentence a = Sentence {sentence :: [TermTree a]}
  deriving (Show, Eq, Functor)

data TermTree a
  = -- | Top level terms with no relations
    Simple (Term a)
  | -- | The last term in a relation.
    Bottom (Term a)
  | -- | A term with an arbitrary number of relations, all terminated by Bottom
    Term a :<- (N.NonEmpty (TermTree a))
  deriving (Show, Eq, Functor)

data Term a = Term {termCriteria :: QueryCriteria, termPattern :: a}
  deriving (Show, Eq, Functor)

{- |
 Return the 'Term` in the given 'TermTree` node.
-}
termTreeNode :: TermTree a -> Term a
termTreeNode b =
  case b of
    Simple t -> t
    Bottom t -> t
    t :<- _ -> t

{- |
 Returns the children of a given node, can be empty in the case of Simple terms.
-}
termTreeChildren :: TermTree a -> [TermTree a]
termTreeChildren (_ :<- ps) = N.toList ps
termTreeChildren _ = []

{- |
 Given a 'TermTree` and a list of trees, insert them as new branches
 immediately under the given node and to the right of any existing predicates.
-}
newPredicates :: TermTree a -> [TermTree a] -> TermTree a
newPredicates basis [] = basis
newPredicates basis ts =
  case basis of
    Simple t -> t :<- N.fromList ts
    Bottom t -> t :<- N.fromList ts
    t :<- ps -> t :<- N.fromList (N.toList ps ++ ts)

-- formatCriteria :: QueryCriteria -> String
-- formatCriteria qc =
--   case qc of
--     DescriptorCriteria -> "D."
--     MetaDescriptorCriteria -> "R."
--     FilePatternCriteria -> "P."
--     UntaggedCriteria -> "U."

-- formatSetOp :: SetOp -> String
-- formatSetOp so =
--   case so of
--     Union -> "U| "
--     Intersect -> "I| "
--     Difference -> "D| "