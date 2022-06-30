{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Component.DescriptorTree.Model.Lens (
  module Component.DescriptorTree.Model.Lens,
) where

import Component.DescriptorTree.Model.Core (
  DescriptorTreeModel,
  DescriptorWithInfo,
 )
import Control.Lens (abbreviatedFields, makeLensesWith)

makeLensesWith abbreviatedFields ''DescriptorTreeModel

makeLensesWith abbreviatedFields ''DescriptorWithInfo