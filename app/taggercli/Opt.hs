{-# OPTIONS_GHC -Wno-typed-holes #-}

module Opt (
  auditDatabase,
) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Data.Foldable as F
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import qualified Data.OccurrenceMap as OM
import qualified Data.Text as T
import Database.Tagger
import Opt.Data
import Opt.Data.Lens
import System.Directory

auditDatabase :: TaggedConnection -> IO TaggerDBAudit
auditDatabase tc =
  flip runReaderT tc $
    mconcat <$> sequence [findMissingFiles, findUnusedDescriptorTrees]

{- |
 Reports all files that do not exist or are impossible to find from the current
 directory.
-}
findMissingFiles ::
  ReaderT
    TaggedConnection
    IO
    TaggerDBAudit
findMissingFiles = do
  tc <- ask
  allDBFiles <- lift $ allFiles tc
  allMissingFiles <-
    filterM
      ( lift
          . fmap not
          . doesFileExist
          . T.unpack
          . filePath
      )
      allDBFiles
  return $ mempty & missingFiles .~ allMissingFiles

{- |
 Reports all Descriptors which are not applied to any file
 nor are any descriptors infra to them.
 Signifying that they are safe to delete from the database.
-}
findUnusedDescriptorTrees ::
  ReaderT
    TaggedConnection
    IO
    TaggerDBAudit
findUnusedDescriptorTrees = do
  tc <- ask
  allDBDescriptors <- lift (allDescriptors tc)
  unusedDescriptorTreeList <-
    F.toList
      <$> execStateT (scanDBDescriptorSet allDBDescriptors) mempty
  return $ mempty & unusedDescriptorTrees .~ unusedDescriptorTreeList
 where
  -- Treats the state as an accumulator as it traverses the given list.
  scanDBDescriptorSet ::
    [Descriptor] ->
    StateT
      (HashSet Descriptor)
      (ReaderT TaggedConnection IO)
      ()
  scanDBDescriptorSet =
    mapM_ mutateIfUnused
   where
    mutateIfUnused ::
      Descriptor ->
      StateT (HashSet Descriptor) (ReaderT TaggedConnection IO) ()
    mutateIfUnused d@(Descriptor dk _) = do
      tc <- lift ask
      infraDescriptors <- liftIO $ getAllInfra dk tc
      infraOccurrences <-
        IM.foldl' (+) 0
          . OM.occurrenceMap
          <$> liftIO
            ( getTagOccurrencesByDescriptorKeys
                (descriptorId <$> infraDescriptors)
                tc
            )
      when (infraOccurrences <= 0) (modify (HS.insert d))