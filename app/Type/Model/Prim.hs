{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Model.Prim
  ( TaggerModel (..),
    SingleFileSelectionModel (..),
    TaggerEvent (..),
    SingleFileEvent (..),
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

import Data.Text (Text)
import Database.Tagger.Access
import Database.Tagger.Type
import Type.Config

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

data TaggerModel = TaggerModel
  { _taggerFileSelection :: ![FileWithTags],
    _taggerFileSetArithmetic :: !FileSetArithmetic,
    _taggerQueryCriteria :: !QueryCriteria,
    _taggerFileSelectionQuery :: !Text,
    _taggerSingleFileModel :: !SingleFileSelectionModel,
    _taggerDescriptorDb :: ![Descriptor],
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

data SingleFileSelectionModel = SingleFileSelectionModel
  { _sfsmSingleFile :: !(Maybe FileWithTags),
    _sfsmTagCounts :: ![TagCount]
  }
  deriving (Show, Eq)

emptySingleFileSelectionModel :: SingleFileSelectionModel
emptySingleFileSelectionModel =
  SingleFileSelectionModel
    { _sfsmSingleFile = Nothing,
      _sfsmTagCounts = []
    }

emptyTaggerModel :: TaggerConfig -> TaggerModel
emptyTaggerModel cfg =
  TaggerModel
    { _taggerFileSelection = [],
      _taggerFileSetArithmetic = Union,
      _taggerQueryCriteria = ByTag,
      _taggerFileSelectionQuery = "",
      _taggerSingleFileModel = emptySingleFileSelectionModel,
      _taggerDescriptorDb = [],
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

class (Enum a, Bounded a, Eq a) => Cyclic a where
  next :: a -> a
  next x = if x == maxBound then minBound else succ x
  prev :: a -> a
  prev x = if x == minBound then maxBound else pred x

data TaggingMode
  = TagMode
  | UntagMode
  deriving (Eq, Read, Enum, Bounded, Cyclic)

isUntagMode :: TaggingMode -> Bool
isUntagMode UntagMode = True
isUntagMode _ = False

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
  | Configure
  deriving (Eq, Show, Enum, Bounded, Cyclic)

data SingleFileEvent
  = SingleFileNextFromFileSelection
  | SingleFilePrevFromFileSelection
  | SingleFilePut !FileWithTags
  | SingleFilePutTagCounts_ ![TagCount]
  | SingleFileGetTagCounts
  | SingleFileMaybePut !(Maybe FileWithTags)
  deriving (Show, Eq)

data TaggerEvent
  = -- Open DB Connection, populate FileDb, DescriptorDb and DescriptorTree with #ALL#
    TaggerInit
  | DoSingleFileEvent !SingleFileEvent
  | -- Update current selection
    FileSelectionUpdate ![FileWithTags]
  | -- Like FileSelectionUpdate but does not rely on FileSetArithmetic
    FileSelectionPut ![FileWithTags]
  | -- Refetch table data for files in selection.
    -- #TODO also refresh the sngle file if there is one
    FileSelectionRefresh_
  | -- Appends some text to the current query, separated by a space.
    FileSelectionAppendQuery !Text
  | -- Send a query to the db, uses _TaggerQueryCriteria to match query type
    -- and _taggerFileSelectionQuery
    FileSelectionCommitQuery
  | -- Clear current selection
    FileSelectionClear
  | FileSelectionQueryClear
  | -- | Set querying set arithmetic to Union, Intersect, or Diff
    FileSetArithmetic !FileSetArithmetic
  | FileSetArithmeticNext
  | FileSetArithmeticPrev
  | -- | Set the query critera which is how files will be queried
    FileSetQueryCriteria !QueryCriteria
  | FileSetQueryCriteriaNext
  | FileSetQueryCriteriaPrev
  | -- Refresh Descriptor DB with argument
    DescriptorDbUpdate ![Descriptor]
  | -- Put the InfraTree of a descriptor
    DescriptorTreePut !DescriptorTree
  | UnrelatedDescriptorTreePut !DescriptorTree
  | -- Put the parent meta tree of the current tree in the model
    DescriptorTreePutParent
  | -- Get a flattened descriptor tree
    DescriptorTreeGet
  | -- Clear the current descriptor tree
    DescriptorTreeClear
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
    PutExtern !()
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
  | ConfigurationExport
  deriving (Show, Eq)
