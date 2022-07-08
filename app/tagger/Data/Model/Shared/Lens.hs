{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Model.Shared.Lens (
  module Data.Model.Shared.Lens,
) where

import Control.Lens
import Data.Model.Shared.Core

makeLensesWith abbreviatedFields ''TextHistory