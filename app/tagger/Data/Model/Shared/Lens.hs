{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Model.Shared.Lens (
  module Data.Model.Shared.Lens,
) where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Data.Model.Shared.Core (VisibilityModel)

makeLensesWith abbreviatedFields ''VisibilityModel