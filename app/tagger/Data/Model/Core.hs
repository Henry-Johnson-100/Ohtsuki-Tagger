{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}

module Data.Model.Core (
  TaggerModel (..),
  createTaggerModel,
  FileSelectionModel (..),
  createFileSelectionModel,
  FileInfo (..),
  createFileInfo,
  FocusedFileModel (..),
  focusedFileDefaultDataFile,
  focusedFileDefaultRecordKey,
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
import Data.OccurrenceHashMap (OccurrenceHashMap)
import qualified Data.OccurrenceHashMap as OHM
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Tagger
import Data.Text (Text)
import Database.Tagger.Type

data TaggerModel = TaggerModel
  { _taggermodelConf :: TaggerConfig
  , _taggermodelDescriptorTreeModel :: DescriptorTreeModel
  , _taggermodelFocusedFileModel :: FocusedFileModel
  , _taggermodelFileSelectionModel :: FileSelectionModel
  , _taggermodelVisibilityModel :: Visibility
  , _taggermodelConnection :: TaggedConnection
  , _taggermodelIsTagMode :: Bool
  , _taggerMassTagText :: Text
  , _taggerShellText :: Text
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
    , _taggermodelFileSelectionModel = createFileSelectionModel
    , _taggermodelVisibilityModel = VisibilityMain
    , _taggermodelConnection = tc
    , _taggermodelIsTagMode = True
    , _taggerMassTagText = ""
    , _taggerShellText = ""
    }

data FileSelectionModel = FileSelectionModel
  { _fileselectionSelection :: Seq File
  , _fileselectionTagOccurrences :: OccurrenceHashMap Descriptor
  , _fileselectionFileSelectionInfoMap :: IntMap FileInfo
  , _fileselectionSetOp :: SetOp
  , _fileselectionQueryText :: Text
  }
  deriving (Show, Eq)

createFileSelectionModel :: FileSelectionModel
createFileSelectionModel =
  FileSelectionModel
    { _fileselectionSelection = S.empty
    , _fileselectionTagOccurrences = OHM.empty
    , _fileselectionFileSelectionInfoMap = IntMap.empty
    , _fileselectionSetOp = Union
    , _fileselectionQueryText = ""
    }

data FileInfo = FileInfo
  { _fileinfoFileInfoRenameText :: Text
  }
  deriving (Show, Eq)

createFileInfo :: FileInfo
createFileInfo = FileInfo ""

data FocusedFileModel = FocusedFileModel
  { _focusedfilemodelFocusedFile :: ConcreteTaggedFile
  , _focusedfilemodelRenderability :: Renderability
  , _focusedfilemodelFocusedFileVis :: Visibility
  , _focusedfilemodelTagText :: Text
  }
  deriving (Show, Eq)

createFocusedFileModel :: Text -> FocusedFileModel
createFocusedFileModel fp =
  FocusedFileModel
    { _focusedfilemodelFocusedFile =
        ConcreteTaggedFile (File focusedFileDefaultRecordKey fp) empty
    , _focusedfilemodelRenderability = RenderingNotSupported
    , _focusedfilemodelFocusedFileVis = VisibilityMain
    , _focusedfilemodelTagText = ""
    }

focusedFileDefaultDataFile :: FilePath
focusedFileDefaultDataFile = "Yui_signature_SS.png"

{- |
 placeholder file id for a default file.
 events for this id should be filtered. (if negative file ids are common in the tagger db
  for some reason though they shouldn't be.)

 \-1
-}
focusedFileDefaultRecordKey :: RecordKey File
focusedFileDefaultRecordKey = -1

data DescriptorTreeModel = DescriptorTreeModel
  { _descriptortreeUnrelatedNode :: Descriptor
  , _descriptortreeUnrelated :: [Descriptor]
  , _descriptortreeFocusedNode :: Descriptor
  , _descriptortreeFocusedTree :: [Descriptor]
  , _descriptortreeDescriptorInfoMap :: IntMap DescriptorInfo
  , _descriptortreeNewDescriptorText :: Text
  , _descriptortreeDescriptorTreeVis :: Visibility
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
  }
  deriving (Show, Eq)

createDescriptorInfo :: DescriptorInfo
createDescriptorInfo = DescriptorInfo False ""

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
    , _descriptortreeDescriptorTreeVis = VisibilityMain
    }