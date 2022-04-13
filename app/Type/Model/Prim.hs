{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Model.Prim
  ( TaggerModel (..),
    TaggerEvent (..),
    FileSetArithmetic (..),
    QueryCriteria (..),
    TaggingMode (..),
    Cyclic (..),
    emptyTaggerModel,
    isUntagMode,
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
    _taggerTagsString :: !Text,
    _taggerTaggingMode :: !TaggingMode,
    _taggerNewDescriptorText :: !Text
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
      _taggerUnrelatedDescriptorTree = NullTree,
      _taggerNewDescriptorText = "",
      _taggerTaggingMode = TagMode
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

data TaggerEvent
  = -- Open DB Connection, populate FileDb, DescriptorDb and DescriptorTree with #ALL#
    TaggerInit
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
  | -- | Set the query critera which is how files will be queried
    FileSetQueryCriteria !QueryCriteria
  | FileSetQueryCriteriaNext
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
  | -- Append Text to the TagsString
    TagsStringAppend !Text
  | DescriptorCommitNewDescriptorText
  | DescriptorDelete !Descriptor
  | DebugPrintSelection
  deriving (Show, Eq)
