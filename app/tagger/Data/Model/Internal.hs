{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Data.Model.Internal (
  TaggerModel (..),
  taggerModel,
  DescriptorTreeModel (..),
  descriptorTreeModel,
) where

import Data.Text (Text)
import Database.Tagger.Type

data TaggerModel = TaggerModel
  { _taggermodelConnection :: TaggedConnection
  , _taggermodelPreviewFocus :: Bool
  , _taggermodelIsTagMode :: Bool
  , _taggerFileSelection :: [File]
  }
  deriving (Show, Eq)

{- |
 Create a new 'TaggerModel` with the given 'TaggedConnection`
-}
taggerModel :: TaggedConnection -> TaggerModel
taggerModel tc =
  TaggerModel
    { _taggermodelConnection = tc
    , _taggermodelPreviewFocus = True
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
descriptorTreeModel :: Descriptor -> DescriptorTreeModel
descriptorTreeModel n =
  DescriptorTreeModel
    { _descriptortreeUnrelated = []
    , _descriptortreeFocusedNode = n
    , _descriptortreeFocusedTree = []
    , _descriptortreeNewDescriptorText = ""
    , _descriptortreeUpdateDescriptorText = ("", "")
    }