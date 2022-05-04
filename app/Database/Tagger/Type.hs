{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Tagger.Type
  ( File (..),
    Descriptor (..),
    FileWithTags (..),
    Tag (..),
    MetaDescriptor (..),
    DescriptorTree (..),
    TagCount (..),
    TagCountMap (..),
    Representative (..),
    FileKey,
    DescriptorKey,
    TagKey,
    tagCountUnmap,
    tagCountMapSumUnion,
    fileWithTagsToTagCountMap,
    insertIntoDescriptorTree,
    descriptorTreeElem,
    flattenTree,
    descriptorTreeChildren,
    constructValidDbPath,
    getPathsToAdd,
    getNode,
    pushTag,
    fwtFileEqual,
    sortChildren,
  )
where

import qualified Control.Monad
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Hashable as H
import qualified Data.List
import qualified Data.Text as T
import Database.SQLite.Simple
import qualified GHC.Generics as Generics
import qualified IO

{-
 ______   ___   _
/ ___\ \ / / \ | |
\___ \\ V /|  \| |
 ___) || | | |\  |
|____/ |_| |_| \_|
-}

type FileKey = Int

type DescriptorKey = Int

type TagKey = Int

type TagCount = (Descriptor, Int)

type TagCountMap = HashMap.HashMap Descriptor Int

tagCountUnmap :: TagCountMap -> [TagCount]
tagCountUnmap = Control.Monad.liftM2 zip HashMap.keys HashMap.elems

tagCountMapSumUnion :: TagCountMap -> TagCountMap -> TagCountMap
tagCountMapSumUnion = HashMap.unionWith (+)

fileWithTagsToTagCountMap :: FileWithTags -> TagCountMap
fileWithTagsToTagCountMap (FileWithTags _ ds) =
  HashMap.fromList . zip ds . Data.List.repeat $ 1

{-
 _____ ___ _     _____
|  ___|_ _| |   | ____|
| |_   | || |   |  _|
|  _|  | || |___| |___
|_|   |___|_____|_____|
-}

data File = File {fileId :: Int, filePath :: T.Text}
  deriving (Show, Eq, Generics.Generic)

instance H.Hashable File where
  hash (File i f) = H.hash $ show i ++ show f

instance Ord File where
  compare (File _ px) (File _ py) = compare px py

instance FromRow File where
  fromRow = File <$> field <*> field

{-
 ____  _____ ____   ____ ____  ___ ____ _____ ___  ____
|  _ \| ____/ ___| / ___|  _ \|_ _|  _ \_   _/ _ \|  _ \
| | | |  _| \___ \| |   | |_) || || |_) || || | | | |_) |
| |_| | |___ ___) | |___|  _ < | ||  __/ | || |_| |  _ <
|____/|_____|____/ \____|_| \_\___|_|    |_| \___/|_| \_\
-}

data Descriptor = Descriptor {descriptorId :: Int, descriptor :: T.Text}
  deriving (Show, Eq, Generics.Generic)

instance H.Hashable Descriptor where
  hash (Descriptor i d) = H.hash $ show i ++ show d

instance Ord Descriptor where
  compare (Descriptor _ dx) (Descriptor _ dy) = compare dx dy

instance FromRow Descriptor where
  fromRow = Descriptor <$> field <*> field

{-
 _____  _    ____
|_   _|/ \  / ___|
  | | / _ \| |  _
  | |/ ___ \ |_| |
  |_/_/   \_\____|
-}

data Tag
  = Tag {tagId :: Int, fileTagId :: Int, descriptorTagId :: Int}
  deriving (Show, Eq, Ord)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

{-
 ____  _____ ____
|  _ \| ____|  _ \
| |_) |  _| | |_) |
|  _ <| |___|  __/
|_| \_\_____|_|
-}

data Representative = Representative
  { repFileId :: !File,
    repDescriptorId :: !Descriptor,
    repDescription :: !(Maybe T.Text)
  }
  deriving (Show, Eq)

{-
 __  __ _____ _____  _    ____  _____ ____
|  \/  | ____|_   _|/ \  |  _ \| ____/ ___|
| |\/| |  _|   | | / _ \ | | | |  _| \___ \
| |  | | |___  | |/ ___ \| |_| | |___ ___) |
|_|  |_|_____| |_/_/   \_\____/|_____|____(_)
-}

data MetaDescriptor = MetaDescriptor
  { metaDescriptorId :: Int,
    infraDescriptorId :: Int
  }
  deriving (Show, Eq)

{-
 _______        _______
|  ___\ \      / /_   _|
| |_   \ \ /\ / /  | |
|  _|   \ V  V /   | |
|_|      \_/\_/    |_|
-}

data FileWithTags = FileWithTags
  { file :: File,
    tags :: [Descriptor]
  }
  deriving (Eq, Generics.Generic)

instance Show FileWithTags where
  show =
    Control.Monad.liftM2
      (++)
      (flip (++) " : " . show . file)
      (concatMap show . Data.List.sort . tags)

instance H.Hashable FileWithTags where
  hash = H.hash . file

fwtFileEqual :: FileWithTags -> FileWithTags -> Bool
(FileWithTags fx _) `fwtFileEqual` (FileWithTags fy _) = fx == fy

pushTag :: FileWithTags -> Descriptor -> FileWithTags
pushTag (FileWithTags f ds) d = FileWithTags f (d : ds)

{-
 ____  _____ ____ _____ ____  _____ _____
|  _ \| ____/ ___|_   _|  _ \| ____| ____|
| | | |  _| \___ \ | | | |_) |  _| |  _|
| |_| | |___ ___) || | |  _ <| |___| |___
|____/|_____|____/ |_| |_| \_\_____|_____|
-}

data DescriptorTree
  = Infra Descriptor
  | Meta Descriptor [DescriptorTree]
  | NullTree
  deriving (Show, Eq)

instance Ord DescriptorTree where
  (<=) NullTree _ = False
  (<=) (Infra d) trb =
    case trb of
      NullTree -> True
      Infra db -> d <= db
      Meta db xs -> null xs && (d <= db)
  (<=) (Meta d cs) trb =
    case trb of
      NullTree -> True
      Infra db -> (not . null) cs || d <= db
      Meta db csb ->
        if null cs
          then null csb && (d <= db)
          else null csb || (length cs >= length csb)

sortChildren :: DescriptorTree -> DescriptorTree
sortChildren tr =
  case tr of
    Meta d cs -> Meta d (Data.List.sort cs)
    _ -> tr

insertIntoDescriptorTree :: DescriptorTree -> DescriptorTree -> DescriptorTree
insertIntoDescriptorTree mt it =
  case mt of
    Infra md -> Meta md [it]
    Meta md cs -> Meta md (it : cs)
    NullTree -> it

descriptorTreeChildren :: DescriptorTree -> [DescriptorTree]
descriptorTreeChildren tr =
  case tr of
    Infra mk -> []
    Meta mk cs -> cs
    NullTree -> []

descriptorTreeElem :: Descriptor -> DescriptorTree -> Bool
descriptorTreeElem k mt =
  case mt of
    Infra mk -> k == mk
    Meta mk cs ->
      (k == mk) || any (descriptorTreeElem k) cs
    NullTree -> False

getNode :: DescriptorTree -> Maybe Descriptor
getNode tr =
  case tr of
    NullTree -> Nothing
    Infra d -> Just d
    Meta d _ -> Just d

flattenTree :: DescriptorTree -> [Descriptor]
flattenTree = flattenTree' []
  where
    flattenTree' :: [Descriptor] -> DescriptorTree -> [Descriptor]
    flattenTree' xs tr =
      case tr of
        Infra d -> d : xs
        Meta d cs -> Data.List.foldl' flattenTree' (d : xs) cs
        NullTree -> []

-- | Validates paths and expands directories
-- Use in Event.Task and not Database.Tagger.Access
getPathsToAdd :: T.Text -> IO [T.Text]
getPathsToAdd p = do
  validated <- runMaybeT . fmap T.unpack . constructValidDbPath $ p
  maybe
    (return [])
    ( \f -> do
        isDir <- IO.doesDirectoryExist f
        (if isDir then returnDirContents else return . (: []) . T.pack) f
    )
    validated
  where
    returnDirContents :: String -> IO [T.Text]
    returnDirContents p = do
      cwd <- IO.getCurrentDirectory
      IO.setCurrentDirectory p
      dirContents <- IO.listDirectory p
      validatedPaths <- mapM (getPathsToAdd . T.pack) dirContents
      IO.setCurrentDirectory cwd
      return . concat $ validatedPaths

-- | A system safe constructor for a File from a string.
-- If the path exists, returns it with its absolute path.
-- Resolves symbolic links
-- Works for paths or directories
constructValidDbPath :: T.Text -> MaybeT IO T.Text
constructValidDbPath rawPath = do
  existsFile <- pathExists rawPath
  resolve existsFile
  where
    pathExists :: T.Text -> MaybeT IO T.Text
    pathExists f' = do
      exists <- Trans.lift . IO.doesPathExist . T.unpack $ f'
      if exists then return f' else (MaybeT . pure) Nothing
    resolve :: T.Text -> MaybeT IO T.Text
    resolve fp' = do
      let fp = T.unpack fp'
      isSymlink <- Trans.lift . IO.pathIsSymbolicLink $ fp
      resolved <-
        if isSymlink then (Trans.lift . IO.getSymbolicLinkTarget) fp else return fp
      absPath <- Trans.lift . IO.makeAbsolute $ resolved
      return . T.pack $ absPath