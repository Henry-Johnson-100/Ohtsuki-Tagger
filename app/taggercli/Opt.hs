{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Opt (
  mainReportAudit,
  auditDatabase,
  reportAudit,
) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State.Strict (
  StateT,
  execStateT,
  modify,
 )
import qualified Data.Foldable as F
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import qualified Data.OccurrenceMap as OM (
  OccurrenceMap (occurrenceMap),
 )
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger
import Opt.Data (TaggerDBAudit)
import Opt.Data.Lens
import System.Directory (
  doesFileExist,
  getCurrentDirectory,
  setCurrentDirectory,
 )
import System.FilePath (takeDirectory)

-- Currently handles opening a db connection when auditing.
-- This may change in the future.
mainReportAudit :: ReaderT TaggedConnection IO ()
mainReportAudit = do
  tc <- ask
  let dbText@(T.unpack -> dbPath) = tc ^. connName
  !curDir <- lift getCurrentDirectory
  let dbDir = takeDirectory dbPath
  lift $ setCurrentDirectory dbDir
  void $
    do
      liftIO . T.IO.putStrLn $ "Running audit on: " <> dbText
      auditResult <- auditDatabase
      liftIO $ reportAudit auditResult
  lift $ setCurrentDirectory curDir

reportAudit :: TaggerDBAudit -> IO ()
reportAudit a = do
  T.IO.putStrLn $
    "Files in database that are unable to be found in the filesystem: "
      <> (T.pack . show . length $ a ^. missingFiles)
  mapM_ (\f -> T.IO.putStrLn $ "\t" <> filePath f) (a ^. missingFiles)
  T.IO.putStrLn ""
  T.IO.putStrLn $
    "Descriptors that are unused and have no InfraDescriptors that are used: "
      <> (T.pack . show . length $ a ^. unusedDescriptorTrees)
  mapM_
    (\d -> T.IO.putStrLn $ "\t" <> descriptor d)
    (a ^. unusedDescriptorTrees)

auditDatabase :: ReaderT TaggedConnection IO TaggerDBAudit
auditDatabase = mconcat <$> sequence [findMissingFiles, findUnusedDescriptorTrees]

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
    sortOn filePath
      <$> filterM
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
    filter
      ( \(Descriptor _ dt) ->
          not
            ("#" `T.isPrefixOf` dt && "#" `T.isSuffixOf` dt)
      )
      . F.toList
      <$> execStateT (scanDBDescriptorSet allDBDescriptors) mempty
  return $ mempty & unusedDescriptorTrees .~ sortOn descriptor unusedDescriptorTreeList
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