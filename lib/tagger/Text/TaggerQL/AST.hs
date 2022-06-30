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
  SentenceTree (..),
  SentenceSet (..),
  Sentence (..),
  TermTree (..),
  ComplexTerm (..),
  SimpleTerm (..),
  Term (..),
  termTreeNode,
  complexTermNode,
  simplifyTermTree,
  newPredicates,
) where

import qualified Data.List.NonEmpty as N
import Data.Tagger (QueryCriteria (..), SetOp (..))

{- |
 A newtype wrapper for a list of 'SentenceTree`s.

 The 'Request` type is the final and most complete representation of a TaggerQL query.
 It essentially consists of a recursive list of queries, all represented in the
 'Request` type as the terminal parent of them all.
-}
newtype Request a = Request {request :: [SentenceTree a]}
  deriving (Show, Eq, Functor)

{- |
 A data type encompassing either a single 'SentenceSet` or a recursive list of
 'SentenceTree`s
-}
data SentenceTree a
  = SentenceNode (SentenceSet a)
  | SentenceBranch SetOp [SentenceTree a]
  deriving (Show, Eq, Functor)

{- |
 A data type encompassing a 'SetOp` Literal and a 'Sentence`

 The 'SetOp` Literal is to inform how this 'Sentence` is combined with others
 in a left-associative combination.
-}
data SentenceSet a
  = CombinableSentence SetOp (Sentence a)
  deriving (Show, Eq, Functor)

{- |
 A list of 'TermTree`s, one 'Sentence` technically makes up a complete TaggerQL query
 but is not implemented this way.
-}
newtype Sentence a = Sentence {sentence :: [TermTree a]}
  deriving (Show, Eq, Functor)

{- |
 A sum of 'SimpleTerm` and 'ComplexTerm`
-}
data TermTree a
  = Simple (SimpleTerm a)
  | Complex (ComplexTerm a)
  deriving (Show, Eq, Functor)

{- |
 A data type that represents either a 'Term` that has 1 or more nested predicates

 or a 'Term` with no predicates that is the bottom of a relationship, and is therefore
 not queried by itself, but only in relation to the 'Term` above it.
-}
data ComplexTerm a
  = Term a :<- (N.NonEmpty (ComplexTerm a))
  | Bottom (Term a)
  deriving (Show, Eq, Functor)

{- |
 newtype wrapper for a 'Term` that signifies a 'Term` is top-level in the request
 and has no predicates.
-}
newtype SimpleTerm a = SimpleTerm (Term a) deriving (Show, Eq, Functor)

{- |
 Smallest unit of a TaggerQL AST.
-}
data Term a = Term {termCriteria :: QueryCriteria, termPattern :: a}
  deriving (Show, Eq, Functor)

{- |
 Return the 'Term` in the given 'TermTree` node.
-}
termTreeNode :: TermTree a -> Term a
termTreeNode b =
  case b of
    Simple (SimpleTerm t) -> t
    (Complex c) -> complexTermNode c

{- |
 Retrieve the 'Term` from a 'ComplexTerm`
-}
complexTermNode :: ComplexTerm a -> Term a
complexTermNode x =
  case x of
    t :<- _ -> t
    Bottom t -> t

{- |
  To the first given 'ComplexTerm`, add a list of predicate 'ComplexTerm`s to
    the right of its current predicates.

  Automatically converts 'Bottom` terms.
-}
newPredicates :: ComplexTerm a -> [ComplexTerm a] -> ComplexTerm a
newPredicates basis [] = basis
newPredicates basis ts =
  case basis of
    Bottom t -> t :<- N.fromList ts
    t :<- ps -> t :<- N.fromList (N.toList ps ++ ts)

{- |
 Converts 'Bottom` 'ComplexTerm`s in a 'TermTree` to 'Simple` nodes.

 Does nothing to existing 'Simple` nodes or ':<-` nodes.
-}
simplifyTermTree :: TermTree a -> TermTree a
simplifyTermTree t@(Simple _) = t
simplifyTermTree t@(Complex c) =
  case c of
    Bottom rawTerm -> Simple . SimpleTerm $ rawTerm
    _ -> t

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