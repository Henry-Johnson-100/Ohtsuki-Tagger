{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Event (
  TaggerEvent (..),
  FileSelectionEvent (..),
  DescriptorTreeEvent (..),
  FocusedFileEvent (..),
) where

import Data.HashSet
import Data.IntMap.Strict (IntMap)
import Data.Model
import Data.OccurrenceHashMap (OccurrenceHashMap)
import Data.Sequence (Seq)
import Data.Text (Text)
import Database.Tagger.Type

data TaggerEvent
  = DoFocusedFileEvent FocusedFileEvent
  | DoFileSelectionEvent FileSelectionEvent
  | DoDescriptorTreeEvent DescriptorTreeEvent
  | TaggerInit
  | RefreshUI
  | ToggleTagMode
  | CloseConnection
  | IOEvent ()
  | ClearTextField (TaggerLens TaggerModel Text)
  deriving (Show, Eq)

data FileSelectionEvent
  = AppendQueryText Text
  | ClearSelection
  | CycleNextFile
  | CyclePrevFile
  | CycleTagOrderCriteria
  | CycleTagOrderDirection
  | MakeFileSelectionInfoMap (Seq File)
  | PutFiles (HashSet File)
  | PutTagOccurrenceHashMap_ (OccurrenceHashMap Descriptor)
  | Query
  | RefreshFileSelection
  | RefreshTagOccurrences
  | -- | Given a Traversable of File keys, fetch an OccurrenceHashMap. Saves having to
    -- call toList on the selection Seq in RefreshTagOccurrences.
    RefreshTagOccurrencesWith (Seq (RecordKey File))
  | ToggleSelectionView
  | TogglePaneVisibility Text
  deriving (Show, Eq)

data FocusedFileEvent
  = DeleteTag (RecordKey Tag)
  | MoveTag ConcreteTag (Maybe (RecordKey Tag))
  | PutConcreteFile_ ConcreteTaggedFile
  | PutFile File
  | RefreshFocusedFileAndSelection
  | TagFile (RecordKey Descriptor) (Maybe (RecordKey Tag))
  | ToggleFocusedFilePaneVisibility Text
  | UnSubTag (RecordKey Tag)
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