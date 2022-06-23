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
  Term (..),
) where

import Data.Tagger (QueryCriteria, SetOp)

{- |
 A collection of 'Sentence`s.

 A 'Request` is a whole TaggerQL query.
-}
newtype Request a = Request [Sentence a] deriving (Show, Eq, Functor)

{- |
 A collection of 'Term`s.
-}
newtype Sentence a = Sentence [Term a] deriving (Show, Eq, Functor)

{- |
 The smallest valid unit of a TaggerQL query.

 It may have a predicate in addition to its basis if the term is a subquery.

 Most complex searches can be desugared into a list of terms.
-}
data Term a = Term
  { -- | How to combine this 'Term` with other results.
    termSetOperation :: SetOp
  , -- | How to search with this 'Term`
    termCriteria :: QueryCriteria
  , -- | The pattern with which to search.
    termBasis :: a
  , -- | The predicate that modifies the search, such as searching for subtags.
    termPredicate :: [Term a]
  }
  deriving (Show, Eq, Functor)
