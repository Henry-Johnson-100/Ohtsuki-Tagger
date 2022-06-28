{-# LANGUAGE StrictData #-}

module Data.Event (
  TaggerEvent (..),
  DescriptorTreeEvent (..),
) where

import Data.Text (Text)
import Database.Tagger.Type

data TaggerEvent
  = DoDescriptorTreeEvent DescriptorTreeEvent
  | ToggleMassOperate
  | ToggleTagMode
  | CloseConnection
  | IOEvent ()
  deriving (Show, Eq)

data DescriptorTreeEvent
  = DescriptorTreeInit
  | RefreshUnrelated
  | PutUnrelated_ [Descriptor]
  | RefreshFocusedTree
  | PutFocusedTree_ Descriptor [Descriptor]
  | RequestFocusedNode Text
  deriving (Show, Eq)