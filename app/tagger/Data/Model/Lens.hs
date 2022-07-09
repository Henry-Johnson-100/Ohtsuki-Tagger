{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.Model.Lens (
  module Data.Model.Lens,
) where

import Control.Lens
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Model.Core

newtype TaggerLens a b = TaggerLens {taggerLens :: Lens' a b}

instance Show (TaggerLens a b) where
  show _ = "Tagger Lens'"

instance Eq (TaggerLens a b) where
  _ == _ = True

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''FocusedFileModel

makeLensesWith abbreviatedFields ''DescriptorInfo

makeLensesWith abbreviatedFields ''FileSelectionModel

makeLensesWith abbreviatedFields ''FileInfo

makeLensesWith abbreviatedFields ''TaggerInfoModel

fileInfoAt :: Int -> Lens' (IntMap FileInfo) FileInfo
fileInfoAt n =
  lens
    (fromMaybe createFileInfo . IntMap.lookup n)
    (flip (IntMap.insert n))

descriptorInfoAt :: Int -> Lens' (IntMap DescriptorInfo) DescriptorInfo
descriptorInfoAt n =
  lens
    (fromMaybe createDescriptorInfo . IntMap.lookup n)
    (flip (IntMap.insert n))