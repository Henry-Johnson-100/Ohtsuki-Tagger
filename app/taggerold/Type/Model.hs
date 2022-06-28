{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Model
  ( module Type.Model.Prim,
    module Type.Model,
  )
where

import Control.Lens
  ( abbreviatedFields,
    makeLenses,
    makeLensesWith,
  )
import Type.Config
  ( DatabaseConfig,
    DescriptorTreeConfig,
    SelectionConfig,
    TaggerConfig,
  )
import Type.Model.Prim

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''FileSelectionModel

makeLensesWith abbreviatedFields ''DescriptorModel

makeLenses ''TaggerConfig

makeLenses ''DatabaseConfig

makeLenses ''SelectionConfig

makeLenses ''DescriptorTreeConfig

makeLenses ''RootedDescriptorTree

makeLenses ''OccurrenceMap

makeLensesWith abbreviatedFields ''TaggedConnection