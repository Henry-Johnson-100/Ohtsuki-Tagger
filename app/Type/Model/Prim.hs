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
import Database.Tagger.Type
  ( Descriptor,
    DescriptorTree (NullTree),
    FileWithTags,
  )

data TaggerModel = TaggerModel
  { _taggerFileSelection :: ![FileWithTags],
    _taggerFileSetArithmetic :: !FileSetArithmetic,
    _taggerQueryCriteria :: !QueryCriteria,
    _taggerFileSelectionQuery :: !Text,
    _taggerFileSingle :: !(Maybe FileWithTags),
    _taggerDescriptorDb :: ![Descriptor],
    _taggerDescriptorTree :: !DescriptorTree,
    _taggerDoSoloTag :: !Bool,
    _taggerConnectionString :: !String
  }
  deriving (Show, Eq)

emptyTaggerModel :: String -> TaggerModel
emptyTaggerModel = TaggerModel [] Union ByTag "" Nothing [] NullTree False

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
  | -- If there is an image in the preview, get it
    FileSingleGet
  | -- Clear the image preview
    FileSingleClear
  | -- Refresh Descriptor DB with argument
    DescriptorDbUpdate ![Descriptor]
  | -- Put the InfraTree of a descriptor
    DescriptorTreePut !DescriptorTree
  | -- Get a flattened descriptor tree
    DescriptorTreeGet
  | -- Clear the current descriptor tree
    DescriptorTreeClear
  | -- Triggers a functionality like 'cycle'
    ToggleDoSoloTag
  | -- Like DescriptorTreePut but looks up a descriptorTree from text
    RequestDescriptorTree !Text
  deriving (Show, Eq)
