{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant return" #-}

module Data.Model.Lens (
  module Data.Model.Lens,
) where

import Control.Lens (
  Lens',
  abbreviatedFields,
  lens,
  makeLensesWith,
 )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Model.Core (
  AddFileModel,
  DescriptorInfo,
  DescriptorTreeModel,
  FileInfo,
  FileSelectionModel,
  FileSelectionTagListModel,
  FocusedFileModel,
  PositioningModel,
  QueryModel,
  TagInputModel,
  TaggerModel,
  createDescriptorInfo,
  createFileInfo,
 )

newtype TaggerLens a b = TaggerLens {taggerLens :: Lens' a b}

instance Show (TaggerLens a b) where
  show :: TaggerLens a b -> String
  show _ = "Tagger Lens'"

instance Eq (TaggerLens a b) where
  (==) :: TaggerLens a b -> TaggerLens a b -> Bool
  _ == _ = True

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''FocusedFileModel

makeLensesWith abbreviatedFields ''TagInputModel

makeLensesWith abbreviatedFields ''DescriptorInfo

makeLensesWith abbreviatedFields ''FileSelectionModel

makeLensesWith abbreviatedFields ''AddFileModel

makeLensesWith abbreviatedFields ''FileSelectionTagListModel

makeLensesWith abbreviatedFields ''FileInfo

makeLensesWith abbreviatedFields ''PositioningModel

makeLensesWith abbreviatedFields ''QueryModel

{-# INLINE fileInfoAt #-}

{- |
 Derived lens for retrieving a 'FileInfo` from an IntMap at the given key.

 If a FileInfo does not exist there, return a default FileInfo
-}
fileInfoAt :: Int -> Lens' (IntMap FileInfo) FileInfo
fileInfoAt n =
  lens
    (fromMaybe createFileInfo . IntMap.lookup n)
    (flip (IntMap.insert n))

{-# INLINE descriptorInfoAt #-}

{- |
 Derived lens for retrieving a 'DescriptorInfo` from an IntMap at the given key

 If one does not exist at that key, return a default DescriptorInfo
-}
descriptorInfoAt :: Int -> Lens' (IntMap DescriptorInfo) DescriptorInfo
descriptorInfoAt n =
  lens
    (fromMaybe createDescriptorInfo . IntMap.lookup n)
    (flip (IntMap.insert n))

{-# INLINE fileSelectionTagListModel #-}

{- |
 Derived lens for retrieving the 'FileSelectionTagListModel` from the
 base 'TaggerModel`

 >fileSelectionModel . tagList
-}
fileSelectionTagListModel :: Lens' TaggerModel FileSelectionTagListModel
fileSelectionTagListModel = fileSelectionModel . tagList