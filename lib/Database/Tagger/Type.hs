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
  DescriptorTree (..),
  sortChildren,
  insertIntoDescriptorTree,
  descriptorTreeChildren,
  descriptorTreeElem,
  getNode,
  flattenTree,

  -- * Lenses
  module Database.Tagger.Type.Lens,
) where

import Data.Hashable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
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

{- |
 Data type that stores hierarchical relations between between descriptors.

 These are created in the MetaDescriptor table and a DescriptorTree
 is just an encoding of a recursive query on that table.
-}
data DescriptorTree
  = -- | A Descriptor that is not meta to any others.
    Infra Descriptor
  | -- | A Descriptor that has 1 or more infra relations.
    -- That is, it is meta to at least one Descriptor.
    Meta Descriptor (NonEmpty.NonEmpty DescriptorTree)
  deriving (Show, Eq)

{- |
 Ignores the children list and just compares the node Descriptor.
-}
instance Ord DescriptorTree where
  (<=) (Infra d) trb =
    case trb of
      Infra db -> d <= db
      Meta db _ -> d <= db
  (<=) (Meta d _) trb =
    case trb of
      Infra db -> d <= db
      Meta db _ -> d <= db

{- |
 Sorts the children trees of the given tree.
-}
sortChildren :: DescriptorTree -> DescriptorTree
sortChildren tr =
  case tr of
    Meta d cs -> Meta d (NonEmpty.sort cs)
    _ -> tr

{- |
 Inserts the given tree into to the head of the
 list of children of the second given tree.

 Will transform an Infra to a Meta.
-}
insertIntoDescriptorTree :: DescriptorTree -> DescriptorTree -> DescriptorTree
insertIntoDescriptorTree mt it =
  case mt of
    Infra md -> Meta md (it :| [])
    Meta md cs -> Meta md (NonEmpty.cons it cs)

{- |
 Return a list of the given tree's children.

 Empty if the tree is Infra.
-}
descriptorTreeChildren :: DescriptorTree -> [DescriptorTree]
descriptorTreeChildren tr =
  case tr of
    Infra _ -> []
    Meta _ cs -> NonEmpty.toList cs

{- |
 Determine if the given Descriptor is contained in the given tree.
-}
descriptorTreeElem :: Descriptor -> DescriptorTree -> Bool
descriptorTreeElem k mt =
  case mt of
    Infra mk -> k == mk
    Meta mk cs ->
      (k == mk) || Prelude.any (descriptorTreeElem k) cs

{- |
 Retrieves the node Descriptor.
-}
getNode :: DescriptorTree -> Descriptor
getNode tr =
  case tr of
    Infra d -> d
    Meta d _ -> d

{- |
 Width-first flatten a tree into a list of Descriptors.

 Will never be empty.
-}
flattenTree :: DescriptorTree -> [Descriptor]
flattenTree = flattenTree' []
 where
  flattenTree' :: [Descriptor] -> DescriptorTree -> [Descriptor]
  flattenTree' xs tr =
    case tr of
      Infra d -> d : xs
      Meta d cs -> L.foldl' flattenTree' (d : xs) cs
