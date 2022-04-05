module Type.Model.Prim
  ( TaggerModel (..),
    TaggerEvent (..),
    emptyTaggerModel,
  )
where

import Database.Tagger.Type
  ( Descriptor,
    DescriptorTree,
    FileWithTags,
  )

data TaggerModel = TaggerModel
  { _taggerFileDb :: [FileWithTags],
    _taggerFileSelection :: [FileWithTags],
    _taggerFileSingle :: Maybe FileWithTags,
    _taggerDescriptorDb :: [Descriptor],
    _taggerDescriptorTree :: Maybe DescriptorTree
  }
  deriving (Show, Eq)

emptyTaggerModel :: TaggerModel
emptyTaggerModel = TaggerModel [] [] Nothing [] Nothing

data TaggerEvent
  = -- Open DB Connection, populate FileDb, DescriptorDb and DescriptorTree with #ALL#
    TaggerInit
  | -- Replace current DB with the given list.
    FileDbUpdate [FileWithTags]
  | -- Union current selection with argument
    FileSelectionUnion [FileWithTags]
  | -- Intersect current selection with argument
    FileSelectionIntersect [FileWithTags]
  | -- Difference current selection with argument
    FileSelectionDiff [FileWithTags]
  | -- Clear current selection
    FileSelectionClear
  | -- Display an image preview
    FileSinglePut FileWithTags
  | -- If there is an image in the preview, get it
    FileSingleGet
  | -- Clear the image preview
    FileSingleClear
  | -- Refresh Descriptor DB with argument
    DescriptorDbUpdate [Descriptor]
  | -- Put the InfraTree of a descriptor
    DescriptorTreePut DescriptorTree
  | -- Get a flattened descriptor tree
    DescriptorTreeGet
  | -- Clear the current descriptor tree
    DescriptorTreeClear
  deriving (Show, Eq)
