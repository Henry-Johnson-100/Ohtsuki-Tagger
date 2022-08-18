{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Engine.QueryEngine.Type (
  SuperTag (..),
  SubTag (..),
  QueryEnv (..),
  IsTag (..),
  Nested (..),
) where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Database.Tagger.Type (Tag, TaggedConnection)
import Text.TaggerQL.AST (Term)

{- |
 Class to polymorphically handle different newtypes of 'Tag`
-}
class IsTag t where
  fromTag :: Tag -> t
  toTag :: t -> Tag

instance IsTag Tag where
  fromTag = id
  toTag = id

newtype SuperTag = SuperTag {superTag :: Tag} deriving (Show, Eq, Hashable)

instance IsTag SuperTag where
  toTag = superTag
  fromTag = SuperTag

newtype SubTag = SubTag {subTag :: Tag} deriving (Show, Eq, Hashable)

instance IsTag SubTag where
  toTag = subTag
  fromTag = SubTag

newtype Nested a = Nested {nested :: a} deriving (Show, Eq, Hashable)

data QueryEnv = QueryEnv
  { queryEnvConn :: TaggedConnection
  , queryEnvSuperTagSet :: HashSet SuperTag
  , queryEnvSubjectTerm :: Term Text
  }
  deriving (Show, Eq)