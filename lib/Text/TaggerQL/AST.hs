{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Text.TaggerQL.AST (
  Term (..),
  Sentence (..),
  TaggerQLAST (..),
) where

import Data.Tagger

{- |
 Represents the entirety of a given TaggerQL query.
-}
data TaggerQLAST a
  = TaggerQLAST [TaggerQLClause a]
  deriving (Show, Eq, Functor, Foldable)

data TaggerQLClause a
  = TaggerQLClause SetOp [TaggerQLToken a]
  deriving (Show, Eq, Functor, Foldable)

data TaggerQLToken a
  = TaggerQLSimpleToken (TaggerQLSimpleTerm a)
  | TaggerQLComplexToken (TaggerQLComplexTerm a)
  deriving (Show, Eq, Functor, Foldable)

data TaggerQLSubClause a
  = TaggerQLSubClause SetOp [TaggerQLToken a]
  deriving (Show, Eq, Functor, Foldable)

data TaggerQLComplexTerm a
  = TaggerQLComplexTerm QueryCriteria a (TaggerQLSubClause a)
  deriving (Show, Eq, Functor, Foldable)

data TaggerQLSimpleTerm a
  = TaggerQLSimpleTerm QueryCriteria a
  | TaggerQLWildCard QueryCriteria
  deriving (Show, Eq, Functor, Foldable)

instance Traversable TaggerQLAST where
  traverse f (TaggerQLAST cs) = TaggerQLAST <$> traverse (traverse f) cs

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
  traverse _ (TaggerQLWildCard qc) = pure (TaggerQLWildCard qc)

{- |
 Data type representing a single term of a TaggerQL query.

 A term is either a wildcard \'*\' or a pattern with SQL escape characters.

 Each term also has an attached 'QueryCriteria` which specifies what the term is meant
 to match against.
-}
data Term a
  = TermPattern QueryCriteria a
  | TermWildCard QueryCriteria
  deriving (Show, Eq)

{- |
 placeholder type that I will be getting rid of soon.
-}
data Sentence a = Sentence SetOp a deriving (Show, Eq)