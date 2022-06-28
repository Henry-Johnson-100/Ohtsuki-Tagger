{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Data.Model.Internal (
  TaggerModel (..),
  createTaggerModel,
  DescriptorTreeModel (..),
  createDescriptorTreeModel,
) where

import Data.Text (Text)
import Database.Tagger.Type

data TaggerModel = TaggerModel
  { _taggermodelDescriptorTreeModel :: DescriptorTreeModel
  , _taggermodelConnection :: TaggedConnection
  , _taggermodelIsMassOperation :: Bool
  , _taggermodelIsTagMode :: Bool
  , _taggerFileSelection :: [File]
  }
  deriving (Show, Eq)

{- |
 Create a new 'TaggerModel` with the given 'TaggedConnection`
-}
createTaggerModel :: TaggedConnection -> Descriptor -> TaggerModel
createTaggerModel tc d =
  TaggerModel
    { _taggermodelDescriptorTreeModel = createDescriptorTreeModel d
    , _taggermodelConnection = tc
    , _taggermodelIsMassOperation = False
    , _taggermodelIsTagMode = True
    , _taggerFileSelection = []
    }

data DescriptorTreeModel = DescriptorTreeModel
  { _descriptortreeUnrelated :: [Descriptor]
  , _descriptortreeFocusedNode :: Descriptor
  , _descriptortreeFocusedTree :: [Descriptor]
  , _descriptortreeNewDescriptorText :: Text
  , _descriptortreeUpdateDescriptorText :: (Text, Text)
  }
  deriving (Show, Eq)

{- |
 Create a new 'DescriptorTreeModel` with the given 'Descriptor` as the parent
 node.
-}
createDescriptorTreeModel :: Descriptor -> DescriptorTreeModel
createDescriptorTreeModel n =
  DescriptorTreeModel
    { _descriptortreeUnrelated = []
    , _descriptortreeFocusedNode = n
    , _descriptortreeFocusedTree = []
    , _descriptortreeNewDescriptorText = ""
    , _descriptortreeUpdateDescriptorText = ("", "")
    }