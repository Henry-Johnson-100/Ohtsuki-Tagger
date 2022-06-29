{-# LANGUAGE RankNTypes #-}
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
  | RefreshUI
  | ToggleMassOperate
  | ToggleTagMode
  | CloseConnection
  | IOEvent ()
  | ClearTextField (TaggerLens TaggerModel Text)
  deriving (Show, Eq)

data DescriptorTreeEvent
  = DescriptorTreeInit
  | RefreshBothDescriptorTrees
  | RefreshUnrelated
  | PutUnrelated_ [DescriptorWithInfo]
  | PutUnrelatedNode_ Descriptor
  | RefreshFocusedTree
  | PutFocusedTree_ Descriptor [DescriptorWithInfo]
  | RequestFocusedNode Text
  | RequestFocusedNodeParent
  | CreateRelation Descriptor Descriptor
  | ToggleDescriptorTreeVisibility Text
  | PutUpdateDescriptorFrom Descriptor
  | UpdateDescriptor
  | InsertDescriptor
  | DeleteDescriptor Descriptor
  deriving (Show, Eq)