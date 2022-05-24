{-# HLINT ignore "Use union" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use intersect" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Type.Model.Prim
  ( TaggerModel (..),
    SingleFileSelectionModel (..),
    FileSelectionModel (..),
    DescriptorModel (..),
    RootedDescriptorTree (..),
    TaggerEvent (..),
    SingleFileEvent (..),
    ConfigurationEvent (..),
    FileSelectionEvent (..),
    TaggedConnectionEvent (..),
    DescriptorEvent (..),
    TaggedConnection (..),
    FileSetArithmetic (..),
    QueryCriteria (..),
    TaggingMode (..),
    ProgramVisibility (..),
    Cyclic (..),
    Listable (..),
    Intersectable (..),
    DescriptorModelTreeLens (..),
    OrderingBy (..),
    OrdDirection (..),
    OrderingMode (..),
    O.Down (..),
    O.comparing,
    emptyTaggerModel,
    isUntagMode,
    plantTree,
  )
where

import Control.Lens
import Control.Monad
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import qualified Data.Ord as O
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Database.Tagger.Type
import Type.BufferList
import Type.Config
import Util.Core

data TaggerModel = TaggerModel
  { _taggerFileSelectionModel :: !FileSelectionModel,
    _taggerSingleFileModel :: !SingleFileSelectionModel,
    _taggerDescriptorModel :: !DescriptorModel,
    _taggerDoSoloTag :: !Bool,
    _taggerDbConn :: !TaggedConnection,
    _taggerTagsString :: !Text,
    _taggerTaggingMode :: !TaggingMode,
    _taggerNewDescriptorText :: !Text,
    _taggerNewFileText :: !Text,
    _taggerProgramConfig :: !TaggerConfig,
    _taggerProgramVisibility :: !ProgramVisibility
  }
  deriving (Show, Eq)

data DescriptorModel = DescriptorModel
  { _dmMainDescriptorTree :: !RootedDescriptorTree,
    _dmUnrelatedDescriptorTree :: !RootedDescriptorTree,
    _dmAllTree :: !RootedDescriptorTree,
    _dmRenameDescriptorFrom :: !Text,
    _dmRenameDescriptorTo :: !Text,
    _dmRepresentativeFile :: !(Maybe Representative)
  }
  deriving (Show, Eq)

-- | A data type to be used in Tagger sub-models
--
-- makes use of lenses
--
-- Contains a text field which has the name of a descriptor. This is used when refreshing
-- the tree to make sure the correct root descriptor is fetched from the database.
--
-- The Ord instance is only an instance of the underlying DescriptorTree and ignores the
-- name field entirely.
data RootedDescriptorTree = RootedDescriptorTree
  { _rootName :: !Text,
    _rootTree :: !DescriptorTree
  }
  deriving (Show, Eq)

instance Ord RootedDescriptorTree where
  compare trx try = compare (_rootTree trx) (_rootTree try)

data FileSelectionModel = FileSelectionModel
  { _fsmFileSelection :: !(BufferList File),
    _fsmSetArithmetic :: !FileSetArithmetic,
    _fsmQueryCriteria :: !QueryCriteria,
    _fsmQueryText :: !Text,
    _fsmSelectionDetailsOrdering :: !OrderingMode
  }
  deriving (Show, Eq)

data SingleFileSelectionModel = SingleFileSelectionModel
  { _sfsmSingleFile :: !(Maybe FileWithTags),
    _sfsmTagCounts :: !(OccurrenceMap Descriptor)
  }
  deriving (Show, Eq)

data OrdDirection = Asc | Desc deriving (Show, Eq, Bounded, Enum, Cyclic)

data OrderingBy = Alphabetical | Numerical deriving (Show, Eq, Bounded, Enum, Cyclic)

data OrderingMode = OrderingMode !OrderingBy !OrdDirection deriving (Show, Eq, Bounded)

instance Show Connection where
  show _ = "Sqlite Connection"

instance Eq Connection where
  x == y = True

data TaggedConnection = TaggedConnection
  { _taggedconnectionConnName :: !Text,
    _taggedconnectionConnInstance :: !(Maybe Connection),
    _taggedconnectionLastAccessed :: !Text,
    _taggedconnectionLastBackup :: !Text
  }
  deriving (Eq)

instance Show TaggedConnection where
  show (TaggedConnection n m _ _) =
    concat
      [ show n,
        " : ",
        maybe "Inactive" (const "Active") m
      ]

class (Enum a, Bounded a, Eq a) => Cyclic a where
  next :: a -> a
  next x = if x == maxBound then minBound else succ x
  prev :: a -> a
  prev x = if x == minBound then maxBound else pred x

class Listable l where
  toList :: l a -> [a]
  fromList :: [a] -> l a

class Intersectable l where
  unionBy :: (a -> a -> Bool) -> l a -> l a -> l a
  union :: Eq a => l a -> l a -> l a
  union x y = unionBy (==) x y
  intersectBy :: (a -> a -> Bool) -> l a -> l a -> l a
  intersect :: Eq a => l a -> l a -> l a
  intersect x y = intersectBy (==) x y
  diffBy :: (a -> a -> Bool) -> l a -> l a -> l a
  diff :: Eq a => l a -> l a -> l a
  diff x y = diffBy (==) x y

instance Listable [] where
  toList = id
  fromList = id

instance Intersectable [] where
  unionBy = L.unionBy
  intersectBy = L.intersectBy
  diffBy = L.deleteFirstsBy

instance Listable BufferList where
  toList = cCollect
  fromList = cFromList

instance Intersectable BufferList where
  unionBy b bl@(BufferList bx xx) (BufferList by xy) =
    let onlyNew xs' = diffBy b xs' (cCollect bl)
     in BufferList (unionBy b bx (onlyNew by)) (unionBy b xx (onlyNew xy))
  intersectBy b (BufferList bx xx) (BufferList by xy) =
    let combinedSelection b' = unionBy b' by xy
     in BufferList
          (intersectBy b bx (combinedSelection b))
          (intersectBy b xx (combinedSelection b))
  diffBy b (BufferList bx xx) (BufferList by xy) =
    let combinedDiff = unionBy b by xy
     in BufferList
          (diffBy b bx combinedDiff)
          (diffBy b xx combinedDiff)

data TaggingMode
  = TagMode
  | UntagMode
  deriving (Eq, Read, Enum, Bounded, Cyclic)

instance Show TaggingMode where
  show TagMode = "Tag"
  show _ = "Untag"

data FileSetArithmetic
  = Union
  | Intersect
  | Diff
  deriving (Show, Eq, Enum, Read, Bounded, Cyclic)

data QueryCriteria
  = ByTag
  | ByPattern
  | ByRelation
  | ByUntagged
  deriving (Eq, Enum, Read, Bounded, Cyclic)

instance Show QueryCriteria where
  show q =
    case q of
      ByTag -> "Tag"
      ByPattern -> "Pattern"
      ByRelation -> "Relation"
      ByUntagged -> "Untagged"

data ProgramVisibility
  = Main
  | Config
  | Database
  | Selection
  | ProgramVisibilityDescriptor
  deriving (Eq, Show, Enum, Bounded, Cyclic)

data SingleFileEvent
  = SingleFileNextFromFileSelection
  | SingleFilePrevFromFileSelection
  | SingleFilePut !FileWithTags
  | SingleFilePutTagCounts_ !(OccurrenceMap Descriptor)
  | SingleFileGetTagCounts
  | SingleFileMaybePut !(Maybe FileWithTags)
  | SingleFileUntag !Tag
  | SingleFileAssociateTag !Tag !Tag
  deriving (Show, Eq)

data TaggedConnectionEvent
  = TaggedConnectionPutLastAccess !Text
  | TaggedConnectionPutLastBackup !Text
  deriving (Show, Eq)

data FileSelectionEvent
  = FileSelectionUpdate ![FileWithTags]
  | FileSelectionPut !(BufferList FileWithTags)
  | FileSelectionBufferPut ![FileWithTags]
  | FileSelectionListPut ![FileWithTags]
  | FileSelectionRefresh_
  | FileSelectionCommitQueryText
  | FileSelectionClear
  | FileSelectionQueryTextClear
  | FileSelectionSetArithmetic !FileSetArithmetic
  | FileSelectionNextSetArithmetic
  | FileSelectionPrevSetArithmetic
  | FileSelectionQueryCriteria !QueryCriteria
  | FileSelectionNextQueryCriteria
  | FileSelectionPrevQueryCriteria
  | FileSelectionShuffle
  | LazyBufferLoad
  | LazyBufferLoadAll
  | LazyBufferFlush
  | FlipInSelectionOrdering
  | CycleInSelectionOrderingBy
  deriving (Show, Eq)

data ConfigurationEvent
  = ExportAll
  deriving (Show, Eq)

-- | A lens used to retrieve a RootedDescriptorTree from a DescriptorModel
--
-- Ex.
--
-- > mainDescriptorTree
type DescriptorModelTreeLens = Lens' DescriptorModel RootedDescriptorTree

data DescriptorEvent
  = DescriptorTreePut !DescriptorModelTreeLens !DescriptorTree
  | DescriptorTreePutParent !DescriptorModelTreeLens
  | RequestDescriptorTree !DescriptorModelTreeLens !Text
  | RefreshDescriptorTree !DescriptorModelTreeLens
  | RenameDescriptor
  | RepresentativeFilePut_ !(Maybe Representative)
  | RepresentativeFileLookup !Descriptor
  | RepresentativeFileClear
  | RepresentativeCreate !File !Descriptor

type TextLens = Lens' TaggerModel Text

data TaggerEvent
  = TaggerInit
  | DoSingleFileEvent !SingleFileEvent
  | DoConfigurationEvent !ConfigurationEvent
  | DoFileSelectionEvent !FileSelectionEvent
  | DoDescriptorEvent !DescriptorEvent
  | DoTaggedConnectionEvent !TaggedConnectionEvent
  | -- Triggers a functionality like 'cycle'
    ToggleDoSoloTag
  | DescriptorCreateRelation ![Descriptor] ![Descriptor]
  | DescriptorUnrelate ![Descriptor]
  | -- Run the text as shell cmd
    ShellCmd
  | -- Receives nothing and does nothing
    IOEvent !()
  | -- Tag the selection with the current tagsString
    TagCommitTagsString
  | TagCommitTagsStringDoSolo
  | TagCommitTagsStringDoSelection
  | TaggingModeNext
  | TaggingModePrev
  | TagsStringClear
  | DescriptorCommitNewDescriptorText
  | DescriptorDelete !Descriptor
  | NewFileTextCommit
  | DatabaseInitialize
  | DatabaseConnect
  | DatabaseBackup
  | DatabaseClose
  | DatabaseConnectionPut_ !TaggedConnection
  | ToggleVisibilityMode !ProgramVisibility
  | forall a. DropTargetAppendText_ TextLens (a -> Text) a
  | UpdateWindowTitleConnectionString !Text !Bool
  | RefreshApplication

emptyDescriptorTreeModel :: DescriptorModel
emptyDescriptorTreeModel =
  DescriptorModel
    { _dmMainDescriptorTree = plantTree NullTree,
      _dmUnrelatedDescriptorTree = (plantTree NullTree) {_rootName = "#UNRELATED#"},
      _dmAllTree = plantTree NullTree,
      _dmRenameDescriptorFrom = "",
      _dmRenameDescriptorTo = "",
      _dmRepresentativeFile = Nothing
    }

emptyFileSelectionModel :: FileSelectionModel
emptyFileSelectionModel =
  FileSelectionModel
    { _fsmFileSelection = emptyBufferList,
      _fsmSetArithmetic = Union,
      _fsmQueryCriteria = ByTag,
      _fsmQueryText = "",
      _fsmSelectionDetailsOrdering = OrderingMode Alphabetical Asc
    }

emptySingleFileSelectionModel :: SingleFileSelectionModel
emptySingleFileSelectionModel =
  SingleFileSelectionModel
    { _sfsmSingleFile = Nothing,
      _sfsmTagCounts = IntMap.empty
    }

emptyTaggerModel :: TaggerConfig -> TaggerModel
emptyTaggerModel cfg =
  TaggerModel
    { _taggerFileSelectionModel = emptyFileSelectionModel,
      _taggerSingleFileModel = emptySingleFileSelectionModel,
      _taggerDescriptorModel = emptyDescriptorTreeModel,
      _taggerDoSoloTag = False,
      _taggerDbConn = TaggedConnection ":memory:" Nothing "Not Available" "Not Available",
      _taggerTagsString = "",
      _taggerNewDescriptorText = "",
      _taggerTaggingMode = TagMode,
      _taggerNewFileText = "",
      _taggerProgramConfig = cfg,
      _taggerProgramVisibility = Main
    }

isUntagMode :: TaggingMode -> Bool
isUntagMode UntagMode = True
isUntagMode _ = False

plantTree :: DescriptorTree -> RootedDescriptorTree
plantTree = RootedDescriptorTree "#ALL#"