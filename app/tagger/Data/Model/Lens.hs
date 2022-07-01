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
import Data.Model.Internal

newtype TaggerLens a b = TaggerLens {taggerLens :: Lens' a b}

instance Show (TaggerLens a b) where
  show _ = "Tagger Lens'"

instance Eq (TaggerLens a b) where
  _ == _ = True

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''FocusedFileModel

makeLensesWith abbreviatedFields ''DescriptorInfo

descriptorInfoMapAt :: Int -> Lens' (IntMap DescriptorInfo) DescriptorInfo
descriptorInfoMapAt n =
  lens
    (fromMaybe createDescriptorInfo . IntMap.lookup n)
    (flip (IntMap.insert n))