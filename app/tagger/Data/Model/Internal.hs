{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}

module Data.Model.Internal (
  TaggerModel (..),
  createTaggerModel,
  FocusedFileModel (..),
  focusedFileDefaultDataFile,
  DescriptorTreeModel (..),
  DescriptorInfo (..),
  createDescriptorInfo,
  createDescriptorTreeModel,
  Renderability (..),
) where

import Data.Config.Internal (TaggerConfig)
import Data.HierarchyMap (empty)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Model.Shared
import Data.Text (Text)
import Database.Tagger.Type

data TaggerModel = TaggerModel
  { _taggermodelConf :: TaggerConfig
  , _taggermodelDescriptorTreeModel :: DescriptorTreeModel
  , _taggermodelFocusedFileModel :: FocusedFileModel
  , _taggermodelVisibilityModel :: VisibilityModel
  , _taggermodelConnection :: TaggedConnection
  , _taggermodelIsMassOperation :: Bool
  , _taggermodelIsTagMode :: Bool
  , _taggerFileSelection :: [File]
  }
  deriving (Show, Eq)

createTaggerModel ::
  TaggerConfig ->
  TaggedConnection ->
  Descriptor ->
  Descriptor ->
  Text ->
  TaggerModel
createTaggerModel conf tc d unRelatedD defaultFilePath =
  TaggerModel
    { _taggermodelConf = conf
    , _taggermodelDescriptorTreeModel = createDescriptorTreeModel d unRelatedD
    , _taggermodelFocusedFileModel = createFocusedFileModel defaultFilePath
    , _taggermodelVisibilityModel = createVisibilityModel
    , _taggermodelConnection = tc
    , _taggermodelIsMassOperation = False
    , _taggermodelIsTagMode = True
    , _taggerFileSelection = []
    }

data FocusedFileModel = FocusedFileModel
  { _focusedfilemodelFocusedFile :: ConcreteTaggedFile
  , _focusedfilemodelRenderability :: Renderability
  }
  deriving (Show, Eq)

createFocusedFileModel :: Text -> FocusedFileModel
createFocusedFileModel fp =
  FocusedFileModel
    { _focusedfilemodelFocusedFile = ConcreteTaggedFile (File (-1) fp) empty
    , _focusedfilemodelRenderability = RenderingNotSupported
    }

focusedFileDefaultDataFile :: FilePath
focusedFileDefaultDataFile = "Yui_signature_SS.png"

data DescriptorTreeModel = DescriptorTreeModel
  { _descriptortreeUnrelatedNode :: Descriptor
  , _descriptortreeUnrelated :: [Descriptor]
  , _descriptortreeFocusedNode :: Descriptor
  , _descriptortreeFocusedTree :: [Descriptor]
  , _descriptortreeDescriptorInfoMap :: IntMap DescriptorInfo
  , _descriptortreeNewDescriptorText :: Text
  }
  deriving (Show, Eq)

data DescriptorWithInfo = DescriptorWithInfo
  { _descriptorwithInfoDescriptor :: Descriptor
  , _descriptorwithinfoDescriptorIsMeta :: Bool
  }
  deriving (Show, Eq)

data DescriptorInfo = DescriptorInfo
  { _descriptorinfoDescriptorIsMeta :: Bool
  , _descriptorinfoRenameText :: Text
  , _descriptorinfoDescriptorInfoVis :: Visibility
  }
  deriving (Show, Eq)

createDescriptorInfo :: DescriptorInfo
createDescriptorInfo = DescriptorInfo False "" VisibilityMain

data Renderability
  = RenderAsImage
  | RenderAsText
  | RenderingNotSupported
  deriving (Show, Eq, Enum)

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
    , _descriptortreeDescriptorInfoMap = IntMap.empty
    , _descriptortreeNewDescriptorText = ""
    }