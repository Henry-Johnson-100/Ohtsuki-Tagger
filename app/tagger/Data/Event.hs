{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Data.Event (
  TaggerEvent (..),
  DescriptorTreeEvent (..),
  FocusedFileEvent (..),
) where

import Data.IntMap.Strict (IntMap)
import Data.Model
import Data.Text (Text)
import Database.Tagger.Type

data TaggerEvent
  = DoFocusedFileEvent FocusedFileEvent
  | DoDescriptorTreeEvent DescriptorTreeEvent
  | TaggerInit
  | RefreshUI
  | ToggleMassOperate
  | ToggleTagMode
  | CloseConnection
  | IOEvent ()
  | ClearTextField (TaggerLens TaggerModel Text)
  deriving (Show, Eq)

data FocusedFileEvent
  = PutFocusedFile File
  | PutFocusedFile_ ConcreteTaggedFile
  deriving (Show, Eq)

data DescriptorTreeEvent
  = CreateRelation Descriptor Descriptor
  | DeleteDescriptor Descriptor
  | DescriptorTreeInit
  | InsertDescriptor
  | PutFocusedTree_ Descriptor [Descriptor] (IntMap DescriptorInfo)
  | PutUnrelated_ [Descriptor] (IntMap DescriptorInfo)
  | PutUnrelatedNode_ Descriptor
  | RefreshBothDescriptorTrees
  | RefreshFocusedTree
  | RefreshUnrelated
  | RequestFocusedNode Text
  | RequestFocusedNodeParent
  | ToggleDescriptorLeafVisibility (RecordKey Descriptor)
  | ToggleDescriptorTreeVisibility Text
  | UpdateDescriptor (RecordKey Descriptor)
  deriving (Show, Eq)