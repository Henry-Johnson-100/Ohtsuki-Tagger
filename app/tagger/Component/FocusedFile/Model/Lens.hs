{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Component.FocusedFile.Model.Lens (
  module Component.FocusedFile.Model.Lens,
) where

import Component.FocusedFile.Model.Core (FocusedFileModel)
import Control.Lens (abbreviatedFields, makeLensesWith)

makeLensesWith abbreviatedFields ''FocusedFileModel