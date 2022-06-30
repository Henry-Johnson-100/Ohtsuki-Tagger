{-# LANGUAGE OverloadedStrings #-}

module Component.DescriptorTree.Model.Core (
  DescriptorTreeModel (..),
  DescriptorWithInfo (..),
  createDescriptorTreeModel,
) where

import Data.Text (Text)
import Database.Tagger.Type (Descriptor)

data DescriptorTreeModel = DescriptorTreeModel
  { _descriptortreeUnrelatedNode :: Descriptor
  , _descriptortreeUnrelated :: [DescriptorWithInfo]
  , _descriptortreeFocusedNode :: Descriptor
  , _descriptortreeFocusedTree :: [DescriptorWithInfo]
  , _descriptortreeNewDescriptorText :: Text
  , _descriptortreeUpdateDescriptorFrom :: Maybe Descriptor
  , _descriptortreeUpdateDescriptorTo :: Text
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
    , _descriptortreeUpdateDescriptorFrom = Nothing
    , _descriptortreeUpdateDescriptorTo = ""
    }