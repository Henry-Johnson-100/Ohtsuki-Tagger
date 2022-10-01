{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Opt.Data.Lens where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Opt.Data

makeLensesWith abbreviatedFields ''TaggerDBAudit

makeLensesWith abbreviatedFields ''TaggerDBStats

makeLensesWith abbreviatedFields ''TaggerCommand