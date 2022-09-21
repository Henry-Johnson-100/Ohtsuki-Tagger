{-# OPTIONS_GHC -Wno-typed-holes #-}

module Opt (
  auditDatabase,
) where

import Control.Lens
import Control.Monad.Trans.Reader
import Database.Tagger
import Opt.Data
import Opt.Data.Lens
import Opt.Parser

auditDatabase :: TaggedConnection -> IO TaggerDBAudit
auditDatabase tc = flip runReaderT tc $ 
  mconcat <$> sequence [findMissingFiles, findUnusedDescriptorTrees]

findMissingFiles ::
  ReaderT
    TaggedConnection
    IO
    TaggerDBAudit
findMissingFiles = undefined

findUnusedDescriptorTrees ::
  ReaderT
    TaggedConnection
    IO
    TaggerDBAudit
findUnusedDescriptorTrees = undefined