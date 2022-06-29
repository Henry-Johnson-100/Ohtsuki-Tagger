{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Data.Model.Internal (
  TaggerModel (..),
  createTaggerModel,
  DescriptorTreeModel (..),
  DescriptorWithInfo (..),
  createDescriptorTreeModel,
) where

import Data.Config (TaggerConfig)
import Data.Text (Text)
import Database.Tagger.Type

data TaggerModel = TaggerModel
  { _taggermodelConf :: TaggerConfig
  , _taggermodelDescriptorTreeModel :: DescriptorTreeModel
  , _taggermodelConnection :: TaggedConnection
  , _taggermodelIsMassOperation :: Bool
  , _taggermodelIsTagMode :: Bool
  , _taggerFileSelection :: [File]
  }
  deriving (Show, Eq)

{- |
 Create a new 'TaggerModel` with the given 'TaggedConnection`
-}
createTaggerModel ::
  TaggerConfig ->
  TaggedConnection ->
  Descriptor ->
  Descriptor ->
  TaggerModel
createTaggerModel conf tc d unRelatedD =
  TaggerModel
    { _taggermodelConf = conf
    , _taggermodelDescriptorTreeModel = createDescriptorTreeModel d unRelatedD
    , _taggermodelConnection = tc
    , _taggermodelIsMassOperation = False
    , _taggermodelIsTagMode = True
    , _taggerFileSelection = []
    }

data DescriptorTreeModel = DescriptorTreeModel
  { _descriptortreeUnrelatedNode :: Descriptor
  , _descriptortreeUnrelated :: [DescriptorWithInfo]
  , _descriptortreeFocusedNode :: Descriptor
  , _descriptortreeFocusedTree :: [DescriptorWithInfo]
  , _descriptortreeNewDescriptorText :: Text
  , _descriptortreeUpdateDescriptorText :: (Text, Text)
  }
  deriving (Show, Eq)

data DescriptorWithInfo = DescriptorWithInfo
  { _descriptorwithInfoDescriptor :: Descriptor
  , _descriptorwithinfoDescriptorIsMeta :: Bool
  }
  deriving (Show, Eq)

{- |
 Create a new 'DescriptorTreeModel` with the given 'Descriptor` as the parent
 node.
-}
createDescriptorTreeModel :: Descriptor -> Descriptor -> DescriptorTreeModel
createDescriptorTreeModel n unrelatedD =
  DescriptorTreeModel
    { _descriptortreeUnrelatedNode = unrelatedD
    , _descriptortreeUnrelated = []
    , _descriptortreeFocusedNode = n
    , _descriptortreeFocusedTree = []
    , _descriptortreeNewDescriptorText = ""
    , _descriptortreeUpdateDescriptorText = ("", "")
    }