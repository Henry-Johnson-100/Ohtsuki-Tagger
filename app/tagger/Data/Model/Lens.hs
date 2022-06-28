{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Model.Lens (
  module Data.Model.Lens,
) where

import Control.Lens
import Data.Model.Internal
import Data.Model.Internal.Config

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''TaggerConfig

makeLensesWith abbreviatedFields ''DatabaseConfig

makeLensesWith abbreviatedFields ''SelectionConfig

makeLensesWith abbreviatedFields ''DescriptorTreeConfig

makeLensesWith abbreviatedFields ''StyleConfig

makeLensesWith abbreviatedFields ''FontConfig

makeLensesWith abbreviatedFields ''WindowConfig