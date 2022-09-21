{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Opt.Data.Lens where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Opt.Data (TaggerDBAudit)

makeLensesWith abbreviatedFields ''TaggerDBAudit