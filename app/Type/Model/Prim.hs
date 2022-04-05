module Type.Model.Prim
  ( TaggerModel (..),
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