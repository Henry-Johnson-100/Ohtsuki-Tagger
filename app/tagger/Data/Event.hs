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
  | ToggleTagMode
  | CloseConnection
  | IOEvent ()
  | ClearTextField (TaggerLens TaggerModel Text)
  deriving (Show, Eq)

data FocusedFileEvent
  = PutConcreteFile_ ConcreteTaggedFile
  | PutFile File
  | TagFile (RecordKey Descriptor) (Maybe (RecordKey Tag))
  | ToggleDetailPaneVisibility
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
  | ToggleDescriptorTreeVisibility Text
  | UpdateDescriptor (RecordKey Descriptor)
  deriving (Show, Eq)