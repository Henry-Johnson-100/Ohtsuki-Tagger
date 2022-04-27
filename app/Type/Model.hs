{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Model
  ( module Type.Model.Prim,
    module Type.Model,
  )
where

import Control.Lens
import qualified Data.Version as Version
import qualified Paths_tagger
import Type.Config
import Type.Model.Prim

taggerVersion :: String
taggerVersion = Version.showVersion Paths_tagger.version

makeLensesWith abbreviatedFields ''TaggerModel

makeLensesWith abbreviatedFields ''SingleFileSelectionModel

makeLensesWith abbreviatedFields ''FileSelectionModel

makeLenses ''TaggerConfig

makeLenses ''DatabaseConfig

makeLenses ''SelectionConfig

makeLenses ''DescriptorTreeConfig