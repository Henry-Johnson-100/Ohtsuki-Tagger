{-# LANGUAGE OverloadedStrings #-}

module Type.Model.Prim
  ( TaggerModel (..),
    TaggerEvent (..),
    FileSetArithmetic (..),
    QueryCriteria (..),
    emptyTaggerModel,
  )
where

import Data.Text (Text)
import Database.Tagger.Access
import Database.Tagger.Type

instance Show Connection where
  show _ = "Sqlite Connection"

instance Eq Connection where
  x == y = True

data TaggerModel = TaggerModel
  { _taggerFileSelection :: ![FileWithTags],
    _taggerFileSetArithmetic :: !FileSetArithmetic,
    _taggerQueryCriteria :: !QueryCriteria,
    _taggerFileSelectionQuery :: !Text,
    _taggerFileSingle :: !(Maybe FileWithTags),
    _taggerDescriptorDb :: ![Descriptor],
    _taggerDescriptorTree :: !DescriptorTree,
    _taggerUnrelatedDescriptorTree :: !DescriptorTree,
    _taggerDoSoloTag :: !Bool,
    _taggerShellCmd :: !Text,
    _taggerExtern :: !(),
    _taggerDbConn :: !Connection,
    _taggerTagsString :: !Text
  }
  deriving (Show, Eq)

emptyTaggerModel :: Connection -> TaggerModel
emptyTaggerModel c =
  TaggerModel
    { _taggerFileSelection = [],
      _taggerFileSetArithmetic = Union,
      _taggerQueryCriteria = ByTag,
      _taggerFileSelectionQuery = "",
      _taggerFileSingle = Nothing,
      _taggerDescriptorDb = [],
      _taggerDescriptorTree = NullTree,
      _taggerDoSoloTag = False,
      _taggerShellCmd = "feh -D120 -zx. -g800x800 -Bwhite",
      _taggerExtern = (),
      _taggerDbConn = c,
      _taggerTagsString = "",
      _taggerUnrelatedDescriptorTree = NullTree
    }

data FileSetArithmetic
  = Union
  | Intersect
  | Diff
  deriving (Show, Eq)

data QueryCriteria
  = ByTag
  | ByPattern
  | ByRelation
  | ByUntagged
  deriving (Eq)

instance Show QueryCriteria where
  show q =
    case q of
      ByTag -> "Tag"
      ByPattern -> "Pattern"
      ByRelation -> "Relation"
      ByUntagged -> "Untagged"

data TaggerEvent
  = -- Open DB Connection, populate FileDb, DescriptorDb and DescriptorTree with #ALL#
    TaggerInit
  | -- Update current selection
    FileSelectionUpdate ![FileWithTags]
  | -- Like FileSelectionUpdate but does not rely on FileSetArithmetic
    FileSelectionPut ![FileWithTags]
  | -- Set model _taggerFileSelectionQuery to the argument.
    FileSelectionStageQuery !Text
  | -- Appends some text to the current query, separated by a space.
    FileSelectionAppendQuery !Text
  | -- Send a query to the db, uses _TaggerQueryCriteria to match query type
    -- and _taggerFileSelectionQuery
    FileSelectionCommitQuery
  | -- Clear current selection
    FileSelectionClear
  | -- | Set querying set arithmetic to Union, Intersect, or Diff
    FileSetArithmetic !FileSetArithmetic
  | -- | Set the query critera which is how files will be queried
    FileSetQueryCriteria !QueryCriteria
  | -- Display an image preview
    FileSinglePut !FileWithTags
  | -- For indeterminate IO
    FileSingleMaybePut !(Maybe FileWithTags)
  | -- If there is an image in the preview, get it
    FileSingleGet
  | -- Clear the image preview
    FileSingleClear
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
  | DescriptorCreateRelation ![Descriptor] ![Descriptor]
  | DescriptorUnrelate ![Descriptor]
  | -- Run the text as shell cmd
    ShellCmd
  | -- Receives nothing and does nothing
    PutExtern !()
  | -- Tag the selection with the current tagsString
    TagCommitTagsString
  | -- Append Text to the TagsString
    TagsStringAppend !Text
  | DebugPrintSelection
  deriving (Show, Eq)
