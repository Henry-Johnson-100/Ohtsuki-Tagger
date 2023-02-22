{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Event (
  TaggerEvent (..),
  TaggerAnonymousEvent (..),
  anonymousEvent,
  FileSelectionEvent (..),
  QueryEvent (..),
  FileSelectionWidgetEvent (..),
  DescriptorTreeEvent (..),
  FocusedFileEvent (..),
  TaggerInfoEvent (..),
) where

import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Model.Core (DescriptorInfo, TaggerModel)
import Data.Model.Lens (TaggerLens)
import Data.Model.Shared (Visibility)
import Data.Model.Shared.Core (TextInput)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import Database.Tagger.Type (
  ConcreteTag,
  ConcreteTaggedFile,
  Descriptor,
  File,
  RecordKey,
  Tag,
 )
import Monomer (AppEventResponse)
import Text.TaggerQL.Expression.AST

data TaggerEvent
  = DoFocusedFileEvent FocusedFileEvent
  | DoFileSelectionEvent FileSelectionEvent
  | DoDescriptorTreeEvent DescriptorTreeEvent
  | DoTaggerInfoEvent TaggerInfoEvent
  | DoQueryEvent QueryEvent
  | TaggerInit
  | ToggleQueryEditMode
  | RefreshUI
  | CloseConnection
  | Unit ()
  | -- | A constructor for producing nested lists of tasks in other tasks.
    -- a slightly more flexible way of calling 'Event` that should be easier to use
    -- in either a 'Task` or normal 'Event` context
    AnonymousEvent [TaggerAnonymousEvent]
  | -- | Existentially quantified event that replaces the given lens
    --  with a Monoid instance with its identity
    forall m. Monoid m => Mempty (TaggerLens TaggerModel m)
  | NextHistory (TaggerLens TaggerModel TextInput)
  | PrevHistory (TaggerLens TaggerModel TextInput)
  | ToggleVisibilityLabel (TaggerLens TaggerModel Visibility) Text
  | -- | Existentially quantified event for appending a string to a string lens.
    forall t. (IsString t, Semigroup t, Eq t) => AppendText (TaggerLens TaggerModel t) t

anonymousEvent :: [AppEventResponse TaggerModel TaggerEvent] -> TaggerEvent
anonymousEvent = AnonymousEvent . fmap TaggerAnonymousEvent

newtype TaggerAnonymousEvent
  = TaggerAnonymousEvent (AppEventResponse TaggerModel TaggerEvent)

instance Eq TaggerAnonymousEvent where
  (==) :: TaggerAnonymousEvent -> TaggerAnonymousEvent -> Bool
  _ == _ = False

instance Show TaggerAnonymousEvent where
  show :: TaggerAnonymousEvent -> String
  show _ = "TaggerAnonymousEvent"

data FileSelectionEvent
  = AddFiles
  | ClearSelection
  | CycleNextFile
  | CycleOrderCriteria
  | CycleOrderDirection
  | CyclePrevFile
  | DeleteFileFromFileSystem (RecordKey File)
  | DoFileSelectionWidgetEvent FileSelectionWidgetEvent
  | IncludeTagListInfraToPattern Text
  | MakeFileSelectionInfoMap (Seq File)
  | PutChunkSequence
  | PutFilesNoCombine (Seq File)
  | PutTagOccurrenceHashMap_ (HashMap Descriptor Int)
  | RefreshFileSelection
  | RefreshSpecificFile (RecordKey File)
  | RefreshSpecificFile_ File
  | RefreshTagOccurrences
  | -- | Given a Traversable of File keys, fetch (HashMap Descriptor Int). Saves having to
    -- call toList on the selection Seq in RefreshTagOccurrences.
    RefreshTagOccurrencesWith (Seq (RecordKey File))
  | RemoveFileFromDatabase (RecordKey File)
  | RemoveFileFromSelection (RecordKey File)
  | RenameFile (RecordKey File)
  | RunSelectionShellCommand
  | ShuffleSelection
  | ToggleSelectionView
  deriving (Show, Eq)

data QueryEvent
  = ClearEditorFocus
  | ParseQuery
  | ReplaceEditorFocus
  | RunEditorFocus
  | RunQuery
  | SetEditorFocus QueryExpression Int

data FileSelectionWidgetEvent
  = CycleNextChunk
  | CyclePrevChunk
  | ResetFileSelectionWidgetChunk
  | ResetFileSelectionWidgetScroll
  deriving (Show, Eq)

data FocusedFileEvent
  = CommitTagText
  | DeleteTag (RecordKey Tag)
  | MoveTag ConcreteTag (Maybe (RecordKey Tag))
  | PutConcreteFile ConcreteTaggedFile
  | PutFile File
  | RefreshFocusedFileAndSelection
  | RunFocusedFileShellCommand
  | TagFile (RecordKey Descriptor) (Maybe (RecordKey Tag))
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
  | UpdateDescriptor (RecordKey Descriptor)
  deriving (Show, Eq)

data TaggerInfoEvent
  = PutLastAccessed Text
  | PutLastSaved Text
  | PutWorkingDirectory Text
  deriving (Show, Eq)