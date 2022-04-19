{-# OPTIONS_GHC -Wno-typed-holes #-}

module Database.Tagger.Type
  ( File (..),
    Descriptor (..),
    FileWithTags (..),
    FileWithTagCounts (..),
    Tag (..),
    MetaDescriptor (..),
    DescriptorTree (..),
    TagCount (..),
    TaggerFile (..),
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
    zipFWT,
  )
where

import qualified Control.Monad
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.List
import qualified Data.Text as T
import qualified IO

newtype TagCount = TagCount {tagCount :: (Descriptor, Int)} deriving (Show, Eq)

data File = File {fileId :: Int, filePath :: T.Text} deriving (Show, Eq)

instance TaggerFile File where
  file = id

instance Ord File where
  compare (File _ px) (File _ py) = compare px py

data Descriptor = Descriptor {descriptorId :: Int, descriptor :: T.Text}
  deriving (Show, Eq)

instance Ord Descriptor where
  compare (Descriptor _ dx) (Descriptor _ dy) = compare dx dy

data Tag = Tag {fileTagId :: Int, descriptorTagId :: Int} deriving (Show, Eq, Ord)

data MetaDescriptor = MetaDescriptor
  { metaDescriptorId :: Int,
    infraDescriptorId :: Int
  }
  deriving (Show, Eq)

data FileWithTagCounts = FileWithTagCounts
  { fileWithTagCountsFile :: File,
    tagCounts :: [TagCount]
  }
  deriving (Show, Eq)

instance TaggerFile FileWithTagCounts where
  file (FileWithTagCounts f _) = f

data FileWithTags = FileWithTags
  { fileWithTagsFile :: File,
    tags :: [Descriptor]
  }
  deriving (Eq)

zipFWT :: FileWithTags -> [TagCount] -> FileWithTagCounts
zipFWT (FileWithTags f _) = FileWithTagCounts f

instance TaggerFile FileWithTags where
  file (FileWithTags f _) = f

class TaggerFile t where
  file :: t -> File

fwtFileEqual :: FileWithTags -> FileWithTags -> Bool
(FileWithTags fx _) `fwtFileEqual` (FileWithTags fy _) = fx == fy

pushTag :: FileWithTags -> Descriptor -> FileWithTags
pushTag (FileWithTags f ds) d = FileWithTags f (d : ds)

-- instance Eq FileWithTags where
--   (FileWithTags fx _) == (FileWithTags fy _) = fx == fy

instance Show FileWithTags where
  show =
    Control.Monad.liftM2
      (++)
      (flip (++) " : " . show . file)
      (concatMap show . Data.List.sort . tags)

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