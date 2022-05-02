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
    emptyTaggerModel,
    isUntagMode,
    plantTree,
  )
where

import Control.Lens
import Control.Monad
import qualified Data.List as L
import Data.Text (Text)
import Database.Tagger.Access
import Database.Tagger.Type
import Type.BufferList
import Type.Config

data TaggerModel = TaggerModel
  { _taggerFileSelectionModel :: !FileSelectionModel,
    _taggerSingleFileModel :: !SingleFileSelectionModel,
    _taggerDescriptorModel :: !DescriptorModel,
    _taggerDoSoloTag :: !Bool,
    _taggerShellCmd :: !Text,
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
  { _fsmFileSelection :: !(BufferList FileWithTags),
    _fsmSetArithmetic :: !FileSetArithmetic,
    _fsmQueryCriteria :: !QueryCriteria,
    _fsmQueryText :: !Text
  }
  deriving (Show, Eq)

data SingleFileSelectionModel = SingleFileSelectionModel
  { _sfsmSingleFile :: !(Maybe FileWithTags),
    _sfsmTagCounts :: ![TagCount]
  }
  deriving (Show, Eq)

instance Show Connection where
  show _ = "Sqlite Connection"

instance Eq Connection where
  x == y = True

data TaggedConnection = TaggedConnection
  { connName :: !Text,
    connInstance :: !(Maybe Connection)
  }
  deriving (Eq)

instance Show TaggedConnection where
  show (TaggedConnection n m) =
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
  unionBy b (BufferList bx xx) (BufferList by xy) =
    BufferList (unionBy b bx by) (unionBy b xx xy)
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
  | SingleFilePutTagCounts_ ![TagCount]
  | SingleFileGetTagCounts
  | SingleFileMaybePut !(Maybe FileWithTags)
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
  | DatabaseConnectionPut_ !TaggedConnection
  | ToggleVisibilityMode !ProgramVisibility
  | forall a. DropTargetAppendText_ TextLens (a -> Text) a

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
      _fsmQueryText = ""
    }

emptySingleFileSelectionModel :: SingleFileSelectionModel
emptySingleFileSelectionModel =
  SingleFileSelectionModel
    { _sfsmSingleFile = Nothing,
      _sfsmTagCounts = []
    }

emptyTaggerModel :: TaggerConfig -> TaggerModel
emptyTaggerModel cfg =
  TaggerModel
    { _taggerFileSelectionModel = emptyFileSelectionModel,
      _taggerSingleFileModel = emptySingleFileSelectionModel,
      _taggerDescriptorModel = emptyDescriptorTreeModel,
      _taggerDoSoloTag = False,
      _taggerShellCmd = "feh -D120 -zx. -g800x800 -Bwhite",
      _taggerDbConn = TaggedConnection ":memory:" Nothing,
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