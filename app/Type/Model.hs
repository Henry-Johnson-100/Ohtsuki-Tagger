{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Model
  ( module Type.Model.Prim,
    fileSelection,
    fileSingle,
    descriptorDb,
    descriptorTree,
    connectionString,
    fileSetArithmetic,
    queryCriteria,
    doSoloTag,
    HasFileSingle,
    HasDoSoloTag,
    HasFileSetArithmetic,
    HasDescriptorTree,
    HasQueryCriteria,
  )
where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Type.Model.Prim

makeLensesWith abbreviatedFields ''TaggerModel