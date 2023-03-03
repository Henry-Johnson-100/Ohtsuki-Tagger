{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Module      : Database.Tagger.Type
Description : Data types used in tagger-lib and tagger-lib dependencies.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.Tagger.Type (
  module Database.Tagger.Type.Prim,
  Database.Tagger.Type.Prim.BareConnection,

  -- * Database Types
  RowId,
  RecordKey (..),
  File (..),
  Descriptor (..),
  Tag (..),
  isSubTag,
  isSubTagOf,
  MetaDescriptor (..),
  DescriptorTree,
  ConcreteTag (..),
  ConcreteTaggedFile (..),

  -- * Lenses
  module Database.Tagger.Type.Lens,
) where

import Data.Hashable (Hashable)
import Data.HierarchyMap (HierarchyMap)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Database.SQLite.Simple (FromRow (..), field)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Database.Tagger.Type.Lens
import Database.Tagger.Type.Prim hiding (BareConnection (..))
import qualified Database.Tagger.Type.Prim
import GHC.Generics (Generic)

{- |
 Representing a Primary Key or RowId for a certain type specified in the type parameter.
-}
newtype RecordKey a = RecordKey Int64
  deriving (Generic)
  deriving newtype (Show, Eq, Hashable, Bounded, Enum, Num, Ord, Real, Read, Integral)

instance FromField (RecordKey a) where
  fromField = fmap RecordKey . fromField

instance ToField (RecordKey a) where
  toField (RecordKey k) = toField k

{- |
 An empty class for types that are identifiable by a RowId or Primary Key.
-}
class RowId r

{- |
 Type synonym for a 'HierarchyMap` of 'Descriptor`s.
-}
type DescriptorTree = HierarchyMap Descriptor

{- |
 Data type representing a single row in the File table.
-}
data File = File
  { -- | Unique primary key identifying a File record.
    fileId :: RecordKey File
  , -- | Unique absolute path to a File.
    filePath :: Text
  }
  deriving (Show, Eq, Generic, Hashable, RowId)

{- |
 Ord instance that ignores the RecordKey.
-}
instance Ord File where
  compare (File _ x) (File _ y) = compare x y

instance FromRow File where
  fromRow = File <$> field <*> field

{- |
  Data type corresponding to a single row in the Descriptor table.
-}
data Descriptor = Descriptor
  { -- | Unique Primary key identifying a Descriptor record.
    descriptorId :: RecordKey Descriptor
  , -- | Descriptor text.
    descriptor :: Text
  }
  deriving (Show, Eq, Generic, Hashable, RowId)

{- |
 Ord instance that ignores the RecordKey.
-}
instance Ord Descriptor where
  compare (Descriptor _ dx) (Descriptor _ dy) = compare dx dy

instance FromRow Descriptor where
  fromRow = Descriptor <$> field <*> field

{- |
 Data type corresponding to a single row in the Tag table.
-}
data Tag = Tag
  { -- | Unique id identifying a single tag record.
    tagId :: RecordKey Tag
  , -- | Foreign key to a File record.
    tagFileId :: RecordKey File
  , -- | Foreign key to a Descriptor record.
    tagDescriptorId :: RecordKey Descriptor
  , -- | Linking key to another Tag record if this tag is a subtag.
    tagSubtagOfId :: Maybe (RecordKey Tag)
  }
  deriving (Show, Eq, Ord, Generic, Hashable, RowId)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field <*> field

{- |
 Returns 'True` if the given 'Tag` is a subtag.
-}
isSubTag :: Tag -> Bool
isSubTag (Tag _ _ _ s) = isJust s

{- |
 Typically used infix.
-}
isSubTagOf :: Tag -> Tag -> Bool
(Tag _ _ _ s) `isSubTagOf` (Tag tid _ _ _) = Just tid == s

{- |
 Data type corresponding to one row in the MetaDescriptor Table.
-}
data MetaDescriptor = MetaDescriptor
  { -- | Foreign key to a Descriptor record.
    metaDescriptorId :: RecordKey Descriptor
  , -- | Foreign key to a Descriptor record.
    infraDescriptorId :: RecordKey Descriptor
  }
  deriving (Show, Eq)

{- |
 Data type corresponding to a \"dereferenced\" 'Tag`.

 Contains the ID of the 'Tag`, its 'Descriptor` and the ID of its supertag if it is
 a subtag.

 Does not contain the 'File` the 'Tag` is attached to because this type is only meant
 for use in the 'ConcreteTaggedFile` where the 'File` would be redundant.
-}
data ConcreteTag = ConcreteTag
  { concreteTagId :: RecordKey Tag
  , concreteTagDescriptor :: Descriptor
  , concreteTagSubTagOfId :: Maybe (RecordKey Tag)
  }
  deriving (Show, Eq, Generic, Hashable)

{- |
 Data type representing a \"dereferenced\" TaggedFile

 Contains the actual file and a 'HierarchyMap` of the 'Descriptor`s it is tagged with.

 For visual or human-readable representation.
-}
data ConcreteTaggedFile = ConcreteTaggedFile
  { concreteTaggedFile :: File
  , concreteTaggedFileDescriptors :: HierarchyMap ConcreteTag
  }
  deriving (Show, Eq)