{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Tagger.Schema where

import Control.Monad.IO.Class
import qualified Data.List
import Database.Groundhog.Sqlite
import Database.Groundhog.TH
import Prelude hiding (FilePath)

data File = File {filePath :: String} deriving (Show, Eq)

-- | Phantom data type for unique filePath constraint
data FilePath v where
  FilePath :: FilePath (UniqueMarker File)

data Descriptor = Descriptor {descriptor :: String} deriving (Show, Eq, Ord)

data DescriptorName v where
  DescriptorName :: DescriptorName (UniqueMarker Descriptor)

-- | Many-to-many relation between File and Descriptors
data Tag = Tag
  { fileTagId :: AutoKey File,
    descriptorTagId :: AutoKey Descriptor
  }

-- | One-to-zero-or-many relationship between Descriptor and Descriptor
data MetaDescriptor = MetaDescriptor
  { metaDescriptorId :: AutoKey Descriptor,
    infraDescriptorId :: AutoKey Descriptor
  }

invertMetaDescriptor :: MetaDescriptor -> MetaDescriptor
invertMetaDescriptor (MetaDescriptor m i) = MetaDescriptor i m

mkPersist
  defaultCodegenConfig
  [groundhog|

- entity: File  
  autoKey:
    constrName: FileId
    default: false
  keys:
    - name: FilePath
      default: true
  constructors:
    - name: File
      uniques:
        - name: FilePath
          fields: [filePath]

- entity: Descriptor
  autoKey:
    constrName: DescriptorId
    default: false
  keys:
    - name: DescriptorName
      default: true
  constructors:
    - name: Descriptor
      uniques:
        - name: DescriptorName
          fields: [descriptor]

- entity: Tag
  autoKey: null
  keys: 
    - name: TagKey
      default: true
  constructors:
    - name: Tag
      fields: 
        - name: fileTagId
          reference:
            onDelete: cascade
        - name: descriptorTagId
          reference:
            onDelete: cascade
      uniques:
        - name: TagKey
          fields: [fileTagId, descriptorTagId]

- entity: MetaDescriptor
  autoKey : null
  keys:
    - name: MetaDescriptorKey
      default: true
  constructors:
    - name: MetaDescriptor
      fields:
        - name: metaDescriptorId
          reference:
            onDelete: cascade
        - name: infraDescriptorId
          reference:
            onDelete: cascade
      uniques:
        - name: MetaDescriptorKey
          fields: [metaDescriptorId, infraDescriptorId]
|]
