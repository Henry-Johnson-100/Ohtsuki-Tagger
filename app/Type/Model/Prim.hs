module Type.Model.Prim
  ( TaggerModel (..),
    TaggerEvent (..),
    FileSetArithmetic (..),
    emptyTaggerModel,
  )
where

import Database.Tagger.Type
  ( Descriptor,
    DescriptorTree,
    FileWithTags,
  )

data TaggerModel = TaggerModel
  { _taggerFileDb :: ![FileWithTags],
    _taggerFileSelection :: ![FileWithTags],
    _taggerFileSetArithmetic :: !FileSetArithmetic,
    _taggerFileSingle :: !(Maybe FileWithTags),
    _taggerDescriptorDb :: ![Descriptor],
    _taggerDescriptorTree :: !(Maybe DescriptorTree),
    _taggerConnectionString :: !String
  }
  deriving (Show, Eq)

emptyTaggerModel :: String -> TaggerModel
emptyTaggerModel = TaggerModel [] [] Union Nothing [] Nothing

data FileSetArithmetic
  = Union
  | Intersect
  | Diff
  deriving (Show, Eq)

data TaggerEvent
  = -- Open DB Connection, populate FileDb, DescriptorDb and DescriptorTree with #ALL#
    TaggerInit
  | -- Replace current DB with the given list.
    FileDbUpdate ![FileWithTags]
  | -- Update current selection
    FileSelectionUpdate ![FileWithTags]
  | -- Clear current selection
    FileSelectionClear
  | -- | Set querying set arithmetic to Union, Intersect, or Diff
    FileSetArithmetic !FileSetArithmetic
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
  deriving (Show, Eq)
