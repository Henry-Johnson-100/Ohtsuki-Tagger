{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Model
  ( module Type.Model.Prim,
    fileSelection,
    fileSingle,
    descriptorDb,
    descriptorTree,
    dbConn,
    fileSetArithmetic,
    queryCriteria,
    fileSelectionQuery,
    doSoloTag,
    shellCmd,
    tagsString,
    unrelatedDescriptorTree,
    newDescriptorText,
    taggingMode,
    HasFileSingle,
    HasDoSoloTag,
    HasFileSetArithmetic,
    HasDescriptorTree,
    HasQueryCriteria,
    HasFileSelectionQuery,
    HasShellCmd,
    HasTagsString,
    HasNewDescriptorText,
    HasTaggingMode,
  )
where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Type.Model.Prim

makeLensesWith abbreviatedFields ''TaggerModel