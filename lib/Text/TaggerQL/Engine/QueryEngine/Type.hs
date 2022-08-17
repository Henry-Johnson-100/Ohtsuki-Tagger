{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Text.TaggerQL.Engine.QueryEngine.Type (
  SuperTag (..),
  SubTag (..),
  QueryEnv (..),
) where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Database.Tagger.Type (Tag, TaggedConnection)

newtype SuperTag = SuperTag {superTag :: Tag} deriving (Show, Eq, Hashable)

newtype SubTag = SubTag {subTag :: Tag} deriving (Show, Eq, Hashable)

data QueryEnv = QueryEnv
  { queryEnvConn :: TaggedConnection
  , queryEnvSuperTagSet :: HashSet SuperTag
  }
  deriving (Show, Eq)