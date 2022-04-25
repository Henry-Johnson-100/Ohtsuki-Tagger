{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Model
  ( module Type.Model.Prim,
    module Type.Model,
  )
where

import Control.Lens
import Type.Config
import Type.Model.Prim

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''SingleFileSelectionModel

makeLensesWith abbreviatedFields ''FileSelectionModel

makeLenses ''TaggerConfig

makeLenses ''DatabaseConfig

makeLenses ''SelectionConfig

makeLenses ''DescriptorTreeConfig