{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Model
  ( module Type.Model.Prim,
    fileSelection,
    singleFileModel,
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
    programConfig,
    programVisibility,
    dbconf,
    dbconfPath,
    dbconfBackup,
    dbconfInit,
    dbconfAutoConnect,
    selectionconf,
    selectionDisplayParents,
    singleFile,
    tagCounts,
    HasSingleFileModel,
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
    HasProgramVisibility,
    HasProgramConfig
  )
where

import Control.Lens
import Type.Config
import Type.Model.Prim

makeLensesWith abbreviatedFields ''TaggerModel

makeLenses ''TaggerConfig

makeLenses ''DatabaseConfig

makeLenses ''SelectionConfig

makeLensesWith abbreviatedFields ''SingleFileSelectionModel