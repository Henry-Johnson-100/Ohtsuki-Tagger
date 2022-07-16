{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Event (
  TaggerEvent (..),
  FileSelectionEvent (..),
  FileSelectionWidgetEvent (..),
  DescriptorTreeEvent (..),
  FocusedFileEvent (..),
  TaggerInfoEvent (..),
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
  | DoTaggerInfoEvent TaggerInfoEvent
  | FocusTagTextField
  | FocusQueryTextField
  | TaggerInit
  | RefreshUI
  | ToggleMainVisibility Text
  | ToggleTagMode
  | CloseConnection
  | IOEvent ()
  | ClearTextField (TaggerLens TaggerModel Text)
  deriving (Show, Eq)

data FileSelectionEvent
  = AddFiles
  | AppendWidgetQueryNode
  | AppendQueryText Text
  | ClearSelection
  | CycleNextFile
  | CycleNextSetOp
  | CyclePrevFile
  | CyclePrevSetOp
  | CycleTagOrderCriteria
  | CycleTagOrderDirection
  | DeleteFileFromFileSystem (RecordKey File)
  | DoFileSelectionWidgetEvent FileSelectionWidgetEvent
  | MakeFileSelectionInfoMap (Seq File)
  | NextAddFileHist
  | NextQueryHist
  | PrevAddFileHist
  | PrevQueryHist
  | PutChunkSequence
  | PutFiles (HashSet File)
  | PutFilesNoCombine (Seq File)
  | PutTagOccurrenceHashMap_ (OccurrenceHashMap Descriptor)
  | Query
  | RefreshFileSelection
  | RefreshSpecificFile (RecordKey File)
  | RefreshSpecificFile_ File
  | RefreshTagOccurrences
  | -- | Given a Traversable of File keys, fetch an OccurrenceHashMap. Saves having to
    -- call toList on the selection Seq in RefreshTagOccurrences.
    RefreshTagOccurrencesWith (Seq (RecordKey File))
  | RemoveFileFromDatabase (RecordKey File)
  | RemoveFileFromSelection (RecordKey File)
  | RenameFile (RecordKey File)
  | ResetAddFileHistIndex
  | ResetQueryHistIndex
  | RunSelectionShellCommand
  | ShuffleSelection
  | ToggleSelectionView
  | TogglePaneVisibility Text
  deriving (Show, Eq)

data FileSelectionWidgetEvent
  = CycleNextChunk
  | CyclePrevChunk
  | ResetFileSelectionWidgetChunk
  | ResetFileSelectionWidgetScroll
  deriving (Show, Eq)

data FocusedFileEvent
  = AppendTagText Text
  | CommitTagText
  | DeleteTag (RecordKey Tag)
  | MoveTag ConcreteTag (Maybe (RecordKey Tag))
  | NextTagHist
  | PrevTagHist
  | PutConcreteFile_ ConcreteTaggedFile
  | PutFile File
  | RefreshFocusedFileAndSelection
  | ResetTagHistIndex
  | RunFocusedFileShellCommand
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

data TaggerInfoEvent
  = PutLastAccessed Text
  | PutLastSaved Text
  | PutWorkingDirectory Text
  deriving (Show, Eq)