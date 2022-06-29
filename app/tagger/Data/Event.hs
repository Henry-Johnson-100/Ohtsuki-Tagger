{-# LANGUAGE StrictData #-}

module Data.Event (
  TaggerEvent (..),
  DescriptorTreeEvent (..),
) where

import Data.Model
import Data.Text (Text)
import Database.Tagger.Type

data TaggerEvent
  = DoDescriptorTreeEvent DescriptorTreeEvent
  | TaggerInit
  | ToggleMassOperate
  | ToggleTagMode
  | CloseConnection
  | IOEvent ()
  deriving (Show, Eq)

data DescriptorTreeEvent
  = DescriptorTreeInit
  | RefreshBothDescriptorTrees
  | RefreshUnrelated
  | PutUnrelated_ [DescriptorWithInfo]
  | RefreshFocusedTree
  | PutFocusedTree_ Descriptor [DescriptorWithInfo]
  | RequestFocusedNode Text
  deriving (Show, Eq)