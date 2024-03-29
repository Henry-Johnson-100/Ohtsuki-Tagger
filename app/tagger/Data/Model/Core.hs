{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Data.Model.Core (
  -- * Data Models
  TaggerModel (..),
  createTaggerModel,
  FileSelectionModel (..),
  AddFileModel (..),
  QueryModel (..),
  getSelectionChunk,
  selectionChunkLength,
  FileSelectionTagListModel (..),
  FileInfo (..),
  createFileInfo,
  constructFileInfo,
  FocusedFileModel (..),
  focusedFileDefaultDataFile,
  focusedFileDefaultRecordKey,
  TagInputModel (..),
  tagInputOptionPaneLabel,
  DescriptorTreeModel (..),
  DescriptorInfo (..),
  createDescriptorInfo,
  createDescriptorTreeModel,
  Renderability (..),
  PositioningModel (..),
  createPositioningModel,
  defaultSelectionAndQueryPositioningModel,
  defaultFileDetailAndDescriptorTreePositioningModel,
) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HierarchyMap (empty)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Model.Shared
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Database.Tagger.Type

data TaggerModel = TaggerModel
  { _taggermodelDescriptorTreeModel :: DescriptorTreeModel
  , _taggermodelFocusedFileModel :: FocusedFileModel
  , _taggermodelFileSelectionModel :: FileSelectionModel
  , _taggermodelTagInputModel :: TagInputModel
  , _taggermodelPositioningModel :: PositioningModel
  , _taggermodelVisibilityModel :: Visibility
  , _taggermodelConnection :: TaggedConnection
  , -- Is a part of the FileSelectionWidget but is logically connected with
    -- the FocusedFile and FileSelection models. I am not willing to combine those two
    -- so this will stay here for now.
    _taggerShellText :: Text
  }
  deriving (Show, Eq)

createTaggerModel ::
  TaggedConnection ->
  Descriptor ->
  Descriptor ->
  Text ->
  TaggerModel
createTaggerModel tc d unRelatedD defaultFilePath =
  TaggerModel
    { _taggermodelDescriptorTreeModel = createDescriptorTreeModel d unRelatedD
    , _taggermodelFocusedFileModel = createFocusedFileModel defaultFilePath
    , _taggermodelFileSelectionModel = createFileSelectionModel
    , _taggermodelPositioningModel = createPositioningModel
    , _taggermodelTagInputModel = createTagInputModel
    , _taggermodelVisibilityModel = VisibilityMain
    , _taggermodelConnection = tc
    , _taggerShellText = ""
    }

data FileSelectionModel = FileSelectionModel
  { _fileselectionTagList :: FileSelectionTagListModel
  , _fileselectionQueryModel :: QueryModel
  , _fileselectionAddFileModel :: AddFileModel
  , _fileselectionSelection :: Seq File
  , _fileselectionCurrentChunk :: Int
  , _fileselectionChunkSize :: Int
  , _fileselectionChunkSequence :: Seq Int
  , _fileselectionFileSelectionInfoMap :: IntMap FileInfo
  , _fileselectionFileSelectionVis :: Visibility
  , _fileselectionIsMassOpMode :: Bool
  , _fileselectionTaggingSelection :: HashSet (RecordKey File)
  }
  deriving (Show, Eq)

createFileSelectionModel :: FileSelectionModel
createFileSelectionModel =
  FileSelectionModel
    { _fileselectionTagList = createFileSelectionTagListModel
    , _fileselectionQueryModel = createQueryModel
    , _fileselectionAddFileModel = createAddFileModel
    , _fileselectionSelection = S.empty
    , _fileselectionCurrentChunk = 0
    , _fileselectionChunkSize = 50
    , _fileselectionChunkSequence = S.singleton 0
    , _fileselectionFileSelectionInfoMap = IntMap.empty
    , _fileselectionFileSelectionVis = VisibilityMain
    , _fileselectionIsMassOpMode = False
    , _fileselectionTaggingSelection = mempty
    }

data AddFileModel = AddFileModel
  { _addfileDirectoryList :: [FilePath]
  , -- | 'VisibilityMain` is a textfield
    --
    -- 'VisibilityAlt` is a list of directories to scan
    _addfileVisibility :: Visibility
  , _addfileInput :: TextInput
  , _addfileInProgress :: Bool
  }
  deriving (Show, Eq)

createAddFileModel :: AddFileModel
createAddFileModel =
  AddFileModel
    { _addfileDirectoryList = []
    , _addfileVisibility = VisibilityMain
    , _addfileInput = createTextInput 10
    , _addfileInProgress = False
    }

data QueryModel = QueryModel
  { _queryInput :: TextInput
  }
  deriving (Show, Eq)

createQueryModel :: QueryModel
createQueryModel =
  QueryModel
    { _queryInput = createTextInput 10
    }

data FileSelectionTagListModel = FileSelectionTagListModel
  { _fileselectiontaglistOccurrences :: HashMap Descriptor Int
  , _fileselectiontaglistOrdering :: OrderBy
  , _fileselectiontaglistInclude :: HashSet (RecordKey Descriptor)
  }
  deriving (Show, Eq)

createFileSelectionTagListModel :: FileSelectionTagListModel
createFileSelectionTagListModel =
  FileSelectionTagListModel
    { _fileselectiontaglistOccurrences = HM.empty
    , _fileselectiontaglistOrdering = OrderBy Numeric Desc
    , _fileselectiontaglistInclude = HS.empty
    }

data FileInfo = FileInfo
  { _fileinfoFileInfoRenameText :: Text
  , _fileinfoDeleteFileIsVis :: Bool
  }
  deriving (Show, Eq)

createFileInfo :: FileInfo
createFileInfo = FileInfo "" False

constructFileInfo :: Text -> FileInfo
constructFileInfo t = FileInfo t False

selectionChunkLength :: TaggerModel -> Int
selectionChunkLength m =
  1
    + ( (Seq.length . _fileselectionSelection . _taggermodelFileSelectionModel $ m)
          `div` (_fileselectionChunkSize . _taggermodelFileSelectionModel $ m)
      )

getSelectionChunk :: TaggerModel -> Seq File
getSelectionChunk m =
  getSelectionChunk'
    (_fileselectionCurrentChunk . _taggermodelFileSelectionModel $ m)
    (_fileselectionChunkSize . _taggermodelFileSelectionModel $ m)
    (_fileselectionSelection . _taggermodelFileSelectionModel $ m)

{- |
 Get the indexed chunk of 50 or fewer items from the given seq.
-}
getSelectionChunk' :: Int -> Int -> Seq a -> Seq a
getSelectionChunk' n cs s
  | Seq.null s = s
  | otherwise =
    let chunks = Seq.chunksOf cs s
        size = Seq.length chunks
     in ( if
              | n >= size -> fromJust . Seq.lookup (size - 1)
              | n < 0 -> fromJust . Seq.lookup 0
              | otherwise -> fromJust . Seq.lookup n
        )
          chunks

data FocusedFileModel = FocusedFileModel
  { _focusedfilemodelFocusedFile :: ConcreteTaggedFile
  , _focusedfilemodelRenderability :: Renderability
  , _focusedfilemodelFocusedFileVis :: Visibility
  }
  deriving (Show, Eq)

createFocusedFileModel :: Text -> FocusedFileModel
createFocusedFileModel fp =
  FocusedFileModel
    { _focusedfilemodelFocusedFile =
        ConcreteTaggedFile (File focusedFileDefaultRecordKey fp) empty
    , _focusedfilemodelRenderability = RenderingNotSupported
    , _focusedfilemodelFocusedFileVis = VisibilityMain
    }

data TagInputModel = TagInputModel
  { _taginputInput :: TextInput
  , _taginputVisibility :: Visibility
  , _taginputIsTagSelection :: Bool
  , _taginputIsTagDelete :: Bool
  }
  deriving (Show, Eq)

createTagInputModel :: TagInputModel
createTagInputModel =
  TagInputModel
    { _taginputInput = createTextInput 10
    , _taginputVisibility = VisibilityMain
    , _taginputIsTagSelection = False
    , _taginputIsTagDelete = False
    }

tagInputOptionPaneLabel :: Text
tagInputOptionPaneLabel = "tagInputOptionPaneLabel"

focusedFileDefaultDataFile :: FilePath
focusedFileDefaultDataFile = "resources/Yui_signature_SS.png"

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
    , _descriptortreeUnrelated = mempty
    , _descriptortreeFocusedNode = n
    , _descriptortreeFocusedTree = mempty
    , _descriptortreeDescriptorInfoMap = IntMap.empty
    , _descriptortreeNewDescriptorText = "Create New Descriptors"
    , _descriptortreeDescriptorTreeVis = VisibilityMain
    }

data PositioningModel = PositioningModel
  { _positioningSelectionAndQueryPosV :: Double
  , _positionSelectionAndQueryPosH :: Double
  , _positionFileDetailAndDescriptorTreePosV :: Double
  , _positionFileDetailAndDescriptorTreePosH :: Double
  }
  deriving (Show, Eq)

createPositioningModel :: PositioningModel
createPositioningModel =
  PositioningModel
    0.5
    0.2
    1.0
    0.8

defaultSelectionAndQueryPositioningModel :: PositioningModel -> PositioningModel
defaultSelectionAndQueryPositioningModel (PositioningModel _ _ ov oh) =
  let (PositioningModel v h _ _) = createPositioningModel
   in PositioningModel v h ov oh

defaultFileDetailAndDescriptorTreePositioningModel :: PositioningModel -> PositioningModel
defaultFileDetailAndDescriptorTreePositioningModel (PositioningModel ov oh _ _) =
  let (PositioningModel _ _ v h) = createPositioningModel
   in PositioningModel ov oh v h
