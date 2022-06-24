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
  Sentence (..),
  Clause (..),
  TermTree (..),
  Term (..),
  newPredicates,
  nestPredicate,
) where

import qualified Data.List.NonEmpty as N
import Data.Tagger (QueryCriteria (..), SetOp (..))

newtype Request a = Request [Sentence a] deriving (Show, Eq, Functor)

data Sentence a = (Clause a) :| (Sentence a) deriving (Show, Eq, Functor)

data Clause a = Clause SetOp (TermTree a) deriving (Show, Eq, Functor)

data TermTree a
  = Simple (Term a)
  | Term a :+ (N.NonEmpty (TermTree a))
  deriving (Show, Eq, Functor)

data Term a = Term QueryCriteria a deriving (Show, Eq, Functor)

{- |
 Given a 'TermTree` and a list of trees, insert them as new branches
 immediately under the given node and to the right of any existing predicates.
-}
newPredicates :: TermTree a -> [TermTree a] -> TermTree a
newPredicates basis [] = basis
newPredicates basis ts =
  case basis of
    Simple t -> t :+ N.fromList ts
    t :+ ps -> t :+ N.fromList (N.toList ps ++ ts)

{- |
 For the first given 'TermTree`, nest the second given tree at the bottom of its
 right-most child.
-}
nestPredicate :: TermTree a -> TermTree a -> TermTree a
nestPredicate basis nest =
  case basis of
    Simple t -> t :+ (nest N.:| [])
    t :+ ps -> t :+ N.fromList (N.init ps ++ [nestPredicate (N.last ps) nest])

formatCriteria :: QueryCriteria -> String
formatCriteria qc =
  case qc of
    DescriptorCriteria -> "D."
    MetaDescriptorCriteria -> "R."
    FilePatternCriteria -> "P."
    UntaggedCriteria -> "U."

formatSetOp :: SetOp -> String
formatSetOp so =
  case so of
    Union -> "U| "
    Intersect -> "I| "
    Difference -> "D| "