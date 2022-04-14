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
    newFileText,
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
    HasNewFileText,
  )
where

import Control.Lens (abbreviatedFields, makeLensesWith)
import Type.Model.Prim

makeLensesWith abbreviatedFields ''TaggerModel