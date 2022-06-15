{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

  -- * Database Types
  RecordKey,
  File (..),
  Descriptor (..),
  Tag (..),
  isSubTag,
  isSubTagOf,
  MetaDescriptor (..),

  -- * Lenses
  module Database.Tagger.Type.Lens,
) where

import Data.Hashable
import Data.Maybe
import Data.Text
import Database.SQLite.Simple
import Database.Tagger.Type.Lens
import Database.Tagger.Type.Prim
import GHC.Generics

type RecordKey = Int

{- |
 Data type representing a single row in the File table.
-}
data File = File
  { -- | Unique primary key identifying a File record.
    fileId :: RecordKey
  , -- | Unique absolute path to a File.
    filePath :: Text
  }
  deriving (Show, Eq, Generic, Hashable)

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
    descriptorId :: RecordKey
  , -- | Descriptor text.
    descriptor :: Text
  }
  deriving (Show, Eq, Generic, Hashable)

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
    tagId :: RecordKey
  , -- | Foreign key to a File record.
    tagFileId :: RecordKey
  , -- | Foreign key to a Descriptor record.
    tagDescriptorId :: RecordKey
  , -- | Linking key to another Tag record if this tag is a subtag.
    tagSubtagOfId :: Maybe RecordKey
  }
  deriving (Show, Eq, Generic, Hashable)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field <*> field

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
    metaDescriptorId :: RecordKey
  , -- | Foreign key to a Descriptor record.
    infraDescriptorId :: RecordKey
  }
  deriving (Show, Eq)