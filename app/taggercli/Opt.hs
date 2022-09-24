{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Opt (
  mainReportAudit,
  auditDatabase,
  reportAudit,
  showStats,
  getConcreteFiles,
  reportTags, 
) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (filterM, unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT (..), ask, asks)
import Control.Monad.Trans.State.Strict (
  StateT,
  execStateT,
  modify,
 )
import qualified Data.Foldable as F
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.HierarchyMap (HierarchyMap, mapHierarchyMap)
import qualified Data.HierarchyMap as HRM
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import Data.Maybe
import qualified Data.OccurrenceMap as OM (
  OccurrenceMap (occurrenceMap),
 )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger (
  Descriptor (..),
  File (filePath),
  HasConnName (connName),
  TaggedConnection,
  allDescriptors,
  allFiles,
  allTags,
  getAllInfra,
  getTagOccurrencesByDescriptorKeys,
  queryForConcreteTaggedFileWithFileId,
 )
import Database.Tagger.Type
import Opt.Data (TaggerDBAudit, TaggerDBStats (..))
import Opt.Data.Lens (
  HasMissingFiles (missingFiles),
  HasUnusedDescriptorTrees (unusedDescriptorTrees),
 )
import System.Directory (
  doesFileExist,
 )

mainReportAudit :: ReaderT TaggedConnection IO ()
mainReportAudit = do
  tc <- ask
  let dbText = tc ^. connName
  void $
    do
      liftIO . T.IO.putStrLn $ "Running audit on: " <> dbText
      auditResult <- auditDatabase
      liftIO $ reportAudit auditResult

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

showStats :: ReaderT TaggedConnection IO ()
showStats = do
  connPath <- asks (^. connName)
  (TaggerDBStats fc dc tc) <- getStats
  liftIO $ do
    T.IO.putStrLn $ "The database, " <> connPath <> ", has:"
    T.IO.putStrLn $ (T.pack . show $ fc) <> " files"
    T.IO.putStrLn $ (T.pack . show $ dc) <> " descriptors"
    T.IO.putStrLn $ (T.pack . show $ tc) <> " applied tags"

getStats :: ReaderT TaggedConnection IO TaggerDBStats
getStats = do
  tc <- ask
  liftIO $ do
    !numberOfFiles <- length <$> allFiles tc
    !numberOfDescriptors <- length <$> allDescriptors tc
    !numberOfTags <- length <$> allTags tc
    return (TaggerDBStats numberOfFiles numberOfDescriptors numberOfTags)

getConcreteFiles :: [File] -> ReaderT TaggedConnection IO [ConcreteTaggedFile]
getConcreteFiles fs = do
  tc <- ask
  liftIO
    . fmap catMaybes
    . mapM (runMaybeT . flip queryForConcreteTaggedFileWithFileId tc)
    $ (fileId <$> fs)

reportTags :: ConcreteTaggedFile -> IO ()
reportTags (ConcreteTaggedFile (File _ fp) hrm) = do
  T.IO.putStrLn fp
  reportHierarchyMap hrm

reportHierarchyMap :: HierarchyMap ConcreteTag -> IO ()
reportHierarchyMap hrm = do
  let ks = HRM.keys hrm
      topLevelMembers = getOnlyTopLevelMembers hrm
  unless (null topLevelMembers)
    . mapM_ (flip runReaderT hrm . reportHierarchyMap' 0)
    $ topLevelMembers
 where
  getOnlyTopLevelMembers hrm' =
    filter (\x -> HRM.metaMember x hrm' && not (HRM.infraMember x hrm')) . HRM.keys $ hrm'
  reportHierarchyMap' :: Int -> ConcreteTag -> ReaderT (HierarchyMap ConcreteTag) IO ()
  reportHierarchyMap' indentLevel ct@(concreteTagDescriptor -> (descriptor -> dp)) = do
    cts@(map (descriptor . concreteTagDescriptor) -> dps) <-
      F.toList <$> (asks . HRM.find $ ct)
    liftIO . T.IO.putStrLn $
      T.replicate (2 * indentLevel) " "
        <> dp
        <> (if null dps then mempty else " {")
    mapM_ (reportHierarchyMap' (indentLevel + 1)) cts
    when (null dps) . liftIO . T.IO.putStrLn $ T.replicate (2 * indentLevel) " " <> "}"