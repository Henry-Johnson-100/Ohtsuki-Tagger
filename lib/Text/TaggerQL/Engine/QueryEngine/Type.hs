{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.TaggerQL.Engine.QueryEngine.Type (
  SuperTag (..),
  SubTag (..),
) where

import Data.Hashable (Hashable)
import Database.Tagger.Type (Tag)

newtype SuperTag = SuperTag {superTag :: Tag} deriving (Show, Eq, Hashable)

newtype SubTag = SubTag {subTag :: Tag} deriving (Show, Eq, Hashable)
