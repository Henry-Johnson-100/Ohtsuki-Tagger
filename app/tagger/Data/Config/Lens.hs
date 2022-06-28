{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Config.Lens (
  module Data.Config.Lens,
) where

import Control.Lens
import Data.Config.Internal

makeLensesWith abbreviatedFields ''TaggerConfig

makeLensesWith abbreviatedFields ''DatabaseConfig

makeLensesWith abbreviatedFields ''SelectionConfig

makeLensesWith abbreviatedFields ''DescriptorTreeConfig

makeLensesWith abbreviatedFields ''StyleConfig

makeLensesWith abbreviatedFields ''FontConfig

makeLensesWith abbreviatedFields ''WindowConfig