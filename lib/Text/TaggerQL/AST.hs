{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Module      : Text.TaggerQL.AST
Description : Defines the components of a TaggerQL AST.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL.AST (
  TaggerQLQuery (..),
  TaggerQLSubQuery (..),
  TaggerQLClause (..),
  TaggerQLToken (..),
  TaggerQLSubClause (..),
  TaggerQLComplexTerm (..),
  TaggerQLSimpleTerm (..),
) where

import Data.Tagger

{- |
 Represents the entirety of a given TaggerQL query.
-}
data TaggerQLQuery a
  = TaggerQLQuery [TaggerQLSubQuery a]
  deriving (Show, Eq, Functor, Foldable)

{- |
 Subqueries are complete queries whose result set's may be combined with
 other subqueries. The default is to combine with the program's set.

 Made up of 'TaggerQLClause` which can be traversed and combined.
-}
data TaggerQLSubQuery a
  = TaggerQLSubQuery [TaggerQLClause a]
  deriving (Show, Eq, Functor, Foldable)

{- |
 A portion of a TaggerQL query that returns a set of results.

 The 'SetOp` determines how it is to be combined with other sets.
-}
data TaggerQLClause a
  = TaggerQLClause SetOp [TaggerQLToken a]
  deriving (Show, Eq, Functor, Foldable)

{- |
 The smallest queryable component of a TaggerQL query.

 One token is all that's required to run a coherent query on a Tagger database
 and return a list of 'File`s
-}
data TaggerQLToken a
  = TaggerQLSimpleToken (TaggerQLSimpleTerm a)
  | TaggerQLComplexToken (TaggerQLComplexTerm a)
  deriving (Show, Eq, Functor, Foldable)

{- |
 The subset of TaggerQL that is permissible in a relational query.

 Specifically, it is the query that goes in the \'{...}\' portion of a relational query.
-}
data TaggerQLSubClause a
  = TaggerQLSubClause SetOp [TaggerQLToken a]
  deriving (Show, Eq, Functor, Foldable)

{- |
 A TaggerQL term that defines a relational query.

 Consists of one basis token and a 'TaggerQLSubClause` that is related to the basis.
-}
data TaggerQLComplexTerm a
  = TaggerQLComplexTerm QueryCriteria a (TaggerQLSubClause a)
  deriving (Show, Eq, Functor, Foldable)

{- |
 Simplest component of a TaggerQL query, can either be a 'QueryCriteria` and text pattern
 or a '*' to denote a wildcard.
-}
data TaggerQLSimpleTerm a
  = TaggerQLSimpleTerm QueryCriteria a
  deriving (Show, Eq, Functor, Foldable)

instance Traversable TaggerQLQuery where
  traverse f (TaggerQLQuery cs) = TaggerQLQuery <$> traverse (traverse f) cs

instance Traversable TaggerQLSubQuery where
  traverse f (TaggerQLSubQuery qs) = TaggerQLSubQuery <$> traverse (traverse f) qs

instance Traversable TaggerQLClause where
  traverse f (TaggerQLClause so ts) = TaggerQLClause so <$> traverse (traverse f) ts

instance Traversable TaggerQLToken where
  traverse f (TaggerQLSimpleToken t) = TaggerQLSimpleToken <$> traverse f t
  traverse f (TaggerQLComplexToken t) = TaggerQLComplexToken <$> traverse f t

instance Traversable TaggerQLSubClause where
  traverse f (TaggerQLSubClause so xs) =
    TaggerQLSubClause so <$> traverse (traverse f) xs

instance Traversable TaggerQLComplexTerm where
  traverse f (TaggerQLComplexTerm qc h ts) =
    TaggerQLComplexTerm qc
      <$> f h
        <*> traverse f ts

instance Traversable TaggerQLSimpleTerm where
  traverse f (TaggerQLSimpleTerm qc t) = TaggerQLSimpleTerm qc <$> f t
