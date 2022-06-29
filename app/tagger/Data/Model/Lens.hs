{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Model.Lens (
  module Data.Model.Lens,
) where

import Control.Lens
import Data.Model.Internal

newtype TaggerLens a b = TaggerLens {taggerLens :: Lens' a b}

instance Show (TaggerLens a b) where
  show _ = "Tagger Lens'"

instance Eq (TaggerLens a b) where
  _ == _ = True

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''FocusedFileModel

makeLensesWith abbreviatedFields ''DescriptorWithInfo

makeLensesWith abbreviatedFields ''VisibilityModel