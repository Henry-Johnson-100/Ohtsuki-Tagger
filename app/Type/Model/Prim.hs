{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Type.Model.Prim
  ( TaggerModel (..),
    SingleFileSelectionModel (..),
    FileSelectionModel (..),
    DescriptorTreeModel (..),
    TaggerEvent (..),
    SingleFileEvent (..),
    ConfigurationEvent (..),
    FileSelectionEvent (..),
    DescriptorTreeEvent (..),
    TaggedConnection (..),
    FileSetArithmetic (..),
    QueryCriteria (..),
    TaggingMode (..),
    ProgramVisibility (..),
    Cyclic (..),
    emptyTaggerModel,
    isUntagMode,
  )
where

import Control.Lens
import Data.Text (Text)
import Database.Tagger.Access
import Database.Tagger.Type
import Type.Config

data TaggerModel = TaggerModel
  { _taggerFileSelectionModel :: !FileSelectionModel,
    _taggerSingleFileModel :: !SingleFileSelectionModel,
    _taggerDescriptorTreeModel :: !DescriptorTreeModel,
    _taggerDescriptorTree :: !DescriptorTree,
    _taggerUnrelatedDescriptorTree :: !DescriptorTree,
    _taggerDoSoloTag :: !Bool,
    _taggerShellCmd :: !Text,
    _taggerExtern :: !(),
    _taggerDbConn :: !TaggedConnection,
    _taggerTagsString :: !Text,
    _taggerTaggingMode :: !TaggingMode,
    _taggerNewDescriptorText :: !Text,
    _taggerNewFileText :: !Text,
    _taggerProgramConfig :: !TaggerConfig,
    _taggerProgramVisibility :: !ProgramVisibility
  }
  deriving (Show, Eq)

data DescriptorTreeModel = DescriptorTreeModel
  { _dtmMainDescriptorTree :: !DescriptorTree,
    _dtmUnrelatedDescriptorTree :: !DescriptorTree,
    _dtmAllTree :: !DescriptorTree
  }
  deriving (Show, Eq)

data FileSelectionModel = FileSelectionModel
  { _fsmFileSelection :: ![FileWithTags],
    _fsmSetArithmetic :: !FileSetArithmetic,
    _fsmQueryCriteria :: !QueryCriteria,
    _fsmQueryText :: !Text
  }
  deriving (Show, Eq)

data SingleFileSelectionModel = SingleFileSelectionModel
  { _sfsmSingleFile :: !(Maybe FileWithTags),
    _sfsmTagCounts :: ![TagCount]
  }
  deriving (Show, Eq)

instance Show Connection where
  show _ = "Sqlite Connection"

instance Eq Connection where
  x == y = True

data TaggedConnection = TaggedConnection
  { connName :: !Text,
    connInstance :: !(Maybe Connection)
  }
  deriving (Eq)

instance Show TaggedConnection where
  show (TaggedConnection n m) =
    concat
      [ show n,
        " : ",
        maybe "Inactive" (const "Active") m
      ]

class (Enum a, Bounded a, Eq a) => Cyclic a where
  next :: a -> a
  next x = if x == maxBound then minBound else succ x
  prev :: a -> a
  prev x = if x == minBound then maxBound else pred x

data TaggingMode
  = TagMode
  | UntagMode
  deriving (Eq, Read, Enum, Bounded, Cyclic)

instance Show TaggingMode where
  show TagMode = "Tag"
  show _ = "Untag"

data FileSetArithmetic
  = Union
  | Intersect
  | Diff
  deriving (Show, Eq, Enum, Read, Bounded, Cyclic)

data QueryCriteria
  = ByTag
  | ByPattern
  | ByRelation
  | ByUntagged
  deriving (Eq, Enum, Read, Bounded, Cyclic)

instance Show QueryCriteria where
  show q =
    case q of
      ByTag -> "Tag"
      ByPattern -> "Pattern"
      ByRelation -> "Relation"
      ByUntagged -> "Untagged"

data ProgramVisibility
  = Main
  | Config
  | Database
  | Selection
  | ProgramVisibilityDescriptor
  deriving (Eq, Show, Enum, Bounded, Cyclic)

data SingleFileEvent
  = SingleFileNextFromFileSelection
  | SingleFilePrevFromFileSelection
  | SingleFilePut !FileWithTags
  | SingleFilePutTagCounts_ ![TagCount]
  | SingleFileGetTagCounts
  | SingleFileMaybePut !(Maybe FileWithTags)
  deriving (Show, Eq)

data FileSelectionEvent
  = FileSelectionUpdate ![FileWithTags]
  | FileSelectionPut ![FileWithTags]
  | FileSelectionRefresh_
  | FileSelectionAppendToQueryText !Text
  | FileSelectionCommitQueryText
  | FileSelectionClear
  | FileSelectionQueryTextClear
  | FileSelectionSetArithmetic !FileSetArithmetic
  | FileSelectionNextSetArithmetic
  | FileSelectionPrevSetArithmetic
  | FileSelectionQueryCriteria !QueryCriteria
  | FileSelectionNextQueryCriteria
  | FileSelectionPrevQueryCriteria
  deriving (Show, Eq)

data ConfigurationEvent
  = ExportAll
  deriving (Show, Eq)

-- | A lens used to retrieve a DescriptorTree from a DescriptorTreeModel
--
-- Ex.
--
-- > mainDescriptorTree
type DescriptorTreeModelLens = Lens' DescriptorTreeModel DescriptorTree

data DescriptorTreeEvent
  = MDescriptorTreePut !DescriptorTreeModelLens !DescriptorTree
  | MDescriptorTreePutParent !DescriptorTreeModelLens
  | MRequestDescriptorTree !DescriptorTreeModelLens !Text
  | MRefreshDescriptorTree !DescriptorTreeModelLens

data TaggerEvent
  = TaggerInit
  | DoSingleFileEvent !SingleFileEvent
  | DoConfigurationEvent !ConfigurationEvent
  | DoFileSelectionEvent !FileSelectionEvent
  | DoDescriptorTreeEvent !DescriptorTreeEvent
  | -- Put the InfraTree of a descriptor
    DescriptorTreePut !DescriptorTree
  | UnrelatedDescriptorTreePut !DescriptorTree
  | -- Put the parent meta tree of the current tree in the model
    DescriptorTreePutParent
  | -- Triggers a functionality like 'cycle'
    ToggleDoSoloTag
  | -- Like DescriptorTreePut but looks up a descriptorTree from text
    RequestDescriptorTree !Text
  | RefreshUnrelatedDescriptorTree
  | RefreshBothDescriptorTrees
  | DescriptorCreateRelation ![Descriptor] ![Descriptor]
  | DescriptorUnrelate ![Descriptor]
  | -- Run the text as shell cmd
    ShellCmd
  | -- Receives nothing and does nothing
    IOEvent !()
  | -- Tag the selection with the current tagsString
    TagCommitTagsString
  | TagCommitTagsStringDoSolo
  | TagCommitTagsStringDoSelection
  | TaggingModeNext
  | TaggingModePrev
  | -- Append Text to the TagsString
    TagsStringAppend !Text
  | TagsStringClear
  | DescriptorCommitNewDescriptorText
  | DescriptorDelete !Descriptor
  | NewFileTextCommit
  | DatabaseInitialize
  | DatabaseConnect
  | DatabaseBackup
  | DatabaseConnectionPut_ !TaggedConnection
  | ToggleVisibilityMode !ProgramVisibility

emptyDescriptorTreeModel :: DescriptorTreeModel
emptyDescriptorTreeModel =
  DescriptorTreeModel
    { _dtmMainDescriptorTree = NullTree,
      _dtmUnrelatedDescriptorTree = NullTree,
      _dtmAllTree = NullTree
    }

emptyFileSelectionModel :: FileSelectionModel
emptyFileSelectionModel =
  FileSelectionModel
    { _fsmFileSelection = [],
      _fsmSetArithmetic = Union,
      _fsmQueryCriteria = ByTag,
      _fsmQueryText = ""
    }

emptySingleFileSelectionModel :: SingleFileSelectionModel
emptySingleFileSelectionModel =
  SingleFileSelectionModel
    { _sfsmSingleFile = Nothing,
      _sfsmTagCounts = []
    }

emptyTaggerModel :: TaggerConfig -> TaggerModel
emptyTaggerModel cfg =
  TaggerModel
    { _taggerFileSelectionModel = emptyFileSelectionModel,
      _taggerSingleFileModel = emptySingleFileSelectionModel,
      _taggerDescriptorTreeModel = emptyDescriptorTreeModel,
      _taggerDescriptorTree = NullTree,
      _taggerDoSoloTag = False,
      _taggerShellCmd = "feh -D120 -zx. -g800x800 -Bwhite",
      _taggerExtern = (),
      _taggerDbConn = TaggedConnection ":memory:" Nothing,
      _taggerTagsString = "",
      _taggerUnrelatedDescriptorTree = NullTree,
      _taggerNewDescriptorText = "",
      _taggerTaggingMode = TagMode,
      _taggerNewFileText = "",
      _taggerProgramConfig = cfg,
      _taggerProgramVisibility = Main
    }

isUntagMode :: TaggingMode -> Bool
isUntagMode UntagMode = True
isUntagMode _ = False