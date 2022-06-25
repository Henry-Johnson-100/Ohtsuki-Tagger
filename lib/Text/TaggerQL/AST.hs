{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Text.TaggerQL.AST
Description : Defines the components of a TaggerQL AST.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.AST (
  CombinableSentence (..),
  Sentence (..),
  CombinableTerm (..),
  TermTree (..),
  Term (..),
  termTreeNode,
  termTreeChildren,
  newPredicates,
) where

import qualified Data.List.NonEmpty as N
import Data.Tagger (QueryCriteria (..), SetOp (..))

data CombinableSentence a = CombinableSentence
  { combinableSentenceSetOp :: SetOp
  , combinableSentence :: Sentence a
  }
  deriving (Show, Eq, Functor)

{- |
 A complete TaggerQL query.
-}
newtype Sentence a = Sentence [CombinableTerm a]
  deriving (Show, Eq, Functor)

{- |
 A single 'TermTree` with a 'SetOp` to tell how the contents of a tree are
 to be combined with other trees.
-}
data CombinableTerm a = CombinableTerm
  { combinableTermSetOp :: SetOp
  , combinableTerm :: TermTree a
  }
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