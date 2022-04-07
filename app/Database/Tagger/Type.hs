{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Eta reduce" #-}
module Database.Tagger.Type
  ( module Database.Tagger.Schema,
    DescriptorAutoKey (..),
    FileAutoKey (..),
    FileWithTags (..),
    Result (..),
    DescriptorTree (..),
    insertIntoDescriptorTree,
    descriptorTreeElem,
    flattenTree,
    descriptorTreeChildren,
    dbFile,
    getNode,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.List
import qualified Database.Groundhog.Sqlite as Sqlite
import Database.Tagger.Schema
import qualified System.Directory as SysDir

-- | A system safe constructor for a File from a string.
-- If the file exists, returns it with its absolute path.
-- Resolves symbolic links
dbFile :: String -> MaybeT IO File
dbFile s = do
  rawFile <- pure . File $ s
  existsFile <- fileExists rawFile
  resolve existsFile
  where
    fileExists :: File -> MaybeT IO File
    fileExists f' = do
      exists <- lift . SysDir.doesFileExist . filePath $ f'
      if exists then return f' else (MaybeT . pure) Nothing
    resolve :: File -> MaybeT IO File
    resolve (File fp) = do
      isSymlink <- lift . SysDir.pathIsSymbolicLink $ fp
      resolved <-
        if isSymlink then (lift . SysDir.getSymbolicLinkTarget) fp else return fp
      absPath <- lift . SysDir.makeAbsolute $ resolved
      return . File $ absPath

-- | Key Descriptor BackendSpecific
type DescriptorAutoKey = Key Descriptor Sqlite.BackendSpecific

-- | Key File BackendSpecific
type FileAutoKey = Key File Sqlite.BackendSpecific

-- | ReaderT Sqlite m a
type Result a = ReaderT.ReaderT Sqlite.Sqlite IO a

data FileWithTags = FileWithTags {file :: File, tags :: [Descriptor]}

instance Eq FileWithTags where
  (FileWithTags fx _) == (FileWithTags fy _) = fx == fy

instance Show FileWithTags where
  show =
    liftM2
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