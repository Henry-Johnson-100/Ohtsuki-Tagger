{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Model.Lens (
  module Data.Model.Lens,
) where

import Control.Lens
import Data.Model.Internal

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''DescriptorWithInfo