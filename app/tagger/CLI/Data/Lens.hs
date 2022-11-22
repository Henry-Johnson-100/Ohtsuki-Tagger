{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module CLI.Data.Lens where

import Control.Lens (abbreviatedFields, makeLensesWith)
import CLI.Data

makeLensesWith abbreviatedFields ''TaggerDBAudit

makeLensesWith abbreviatedFields ''TaggerDBStats

makeLensesWith abbreviatedFields ''TaggerCommand

makeLensesWith abbreviatedFields ''TaggerQueryCommand
makeLensesWith abbreviatedFields ''TaggerDBCommand