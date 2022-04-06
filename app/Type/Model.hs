{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Model
  ( module Type.Model.Prim,
    fileDb,
    fileSelection,
    fileSingle,
    descriptorDb,
    descriptorTree,
    connectionString,
    fileSetArithmetic,
    doSoloTag,
    HasFileSingle,
  )
where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Type.Model.Prim

makeLensesWith abbreviatedFields ''TaggerModel