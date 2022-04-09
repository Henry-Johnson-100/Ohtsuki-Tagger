module Database.Tagger.Type
  ( File (..),
    Descriptor (..),
    FileWithTags (..),
    Tag (..),
    MetaDescriptor (..),
    DescriptorTree (..),
    insertIntoDescriptorTree,
    descriptorTreeElem,
    flattenTree,
    descriptorTreeChildren,
    validatePath,
    getNode,
    pushTag,
    fwtFileEqual,
  )
where

import qualified Control.Monad
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Data.List
import qualified Data.Text as T
import qualified System.Directory as SysDir

data File = File {fileId :: Int, filePath :: T.Text} deriving (Show, Eq)

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

data FileWithTags = FileWithTags {file :: File, tags :: [Descriptor]} deriving (Eq)

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

-- | A system safe constructor for a File from a string.
-- If the file exists, returns it with its absolute path.
-- Resolves symbolic links
validatePath :: T.Text -> MaybeT IO T.Text
validatePath rawPath = do
  existsFile <- fileExists rawPath
  resolve existsFile
  where
    fileExists :: T.Text -> MaybeT IO T.Text
    fileExists f' = do
      exists <- Trans.lift . SysDir.doesFileExist . T.unpack $ f'
      if exists then return f' else (MaybeT . pure) Nothing
    resolve :: T.Text -> MaybeT IO T.Text
    resolve fp' = do
      let fp = T.unpack fp'
      isSymlink <- Trans.lift . SysDir.pathIsSymbolicLink $ fp
      resolved <-
        if isSymlink then (Trans.lift . SysDir.getSymbolicLinkTarget) fp else return fp
      absPath <- Trans.lift . SysDir.makeAbsolute $ resolved
      return . T.pack $ absPath