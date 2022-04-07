{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Tagger.Access
  ( tag,
    relate,
    deleteRelation,
    addDescriptor,
    safeAddFile,
    lookupDescriptorAutoKey,
    getFileWithTags,
    getDescriptor,
    fetchInfraTree,
    fetchAllIndexedDescriptors,
    fetchMetaTree,
    fetchFilesWithTag,
    fetchFilesInTree,
    fetchAllTags,
    fetchTaggedFiles,
    fetchUntaggedFiles,
    fetchAllFiles,
    fetchGetFile,
    fetchFileDescriptors,
    emptyResult,
    connectThenRun,
    Sqlite.withSqliteConn,
    Sqlite.runDbConn,
    Sqlite.runMigration,
    createDatabaseMigration,
  )
where

import Control.Monad (unless, when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.Control as Control
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Control.Monad.Trans.State.Lazy as StateT hiding (get)
import qualified Data.List
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import qualified Database.Groundhog.Core as Groundhog
import qualified Database.Groundhog.Generic.Sql.Functions as Groundhog.Sql.Functions
import Database.Groundhog.Sqlite ((&&.), (=.), (==.))
import qualified Database.Groundhog.Sqlite as Sqlite
import Database.Tagger.Type
  ( Descriptor (descriptor),
    DescriptorAutoKey,
    DescriptorName (DescriptorName),
    DescriptorTree (Infra, NullTree),
    Field
      ( DescriptorField,
        DescriptorTagIdField,
        FileTagIdField,
        InfraDescriptorIdField,
        MetaDescriptorIdField
      ),
    File (filePath),
    FileAutoKey,
    FilePath (FilePath),
    FileWithTags (FileWithTags),
    Key (MetaDescriptorKeyKey),
    MetaDescriptor (MetaDescriptor),
    MetaDescriptorKey (MetaDescriptorKey),
    Result,
    Tag (fileTagId),
    TagKey (TagKey),
    dbFile,
    descriptorTreeElem,
    flattenTree,
    insertIntoDescriptorTree,
  )
import System.IO (hPutStrLn, stderr)

errOut :: String -> ReaderT.ReaderT Sqlite.Sqlite IO ()
errOut = liftIO . hPutStrLn stderr

maybeTToResult :: MaybeT IO a -> Result (Maybe a)
maybeTToResult = lift . runMaybeT

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

-- | Return either one
eitherOne :: Either c c -> c
eitherOne = either id id

-- | Ignores Unique constraint violations.
-- Immediately relates new descriptors to #UNRELATED# unless
-- it is unable to find #UNRELATED#
addDescriptor :: Descriptor -> Result DescriptorAutoKey
addDescriptor d = do
  dk <- Sqlite.insertBy DescriptorName >=> return . eitherOne $ d
  unrelation <- lookupDescriptorAutoKey "#UNRELATED#"
  maybe
    (return dk)
    ( \mk -> do
        relate (MetaDescriptor mk dk)
        return dk
    )
    unrelation

-- | Ignores Unique constraint errors and returns old key if there is a violation.
addFile :: File -> Result FileAutoKey
addFile = Sqlite.insertBy FilePath >=> return . eitherOne

-- | Adds file if it is a valid file name and it exists.
-- returns Nothing if it does not satisfy.
safeAddFile :: File -> Result (Maybe FileAutoKey)
safeAddFile =
  maybeTToResult
    . dbFile
    . filePath
    >=> maybe
      (return Nothing)
      (addFile >=> return . Just)

emptyResult :: Result ()
emptyResult = return ()

tag :: Tag -> Result ()
tag = Sqlite.insertBy TagKey >=> return . eitherOne

-- | Create a relation for a (MetaDescriptor x y)
-- iff y is not meta to x && There are no current relations of any descriptor z to y
--
-- If a tag is related to #UNRELATED# then that relation is deleted before forming
-- the new one.
relate :: MetaDescriptor -> Result ()
relate mdes =
  case mdes of
    MetaDescriptor x y -> do
      metaToXTree <- fetchMetaTree x
      yDescriptor <- getDescriptor y
      countExistingRelations <- Sqlite.count (y ==. InfraDescriptorIdField)
      markedUnrelated <- isMarkedUnrelated y
      unless
        (maybe False (`descriptorTreeElem` metaToXTree) yDescriptor)
        ( when
            (countExistingRelations <= 1)
            ( do
                when
                  (fst markedUnrelated)
                  (deleteRelation . MetaDescriptor (fromJust . snd $ markedUnrelated) $ y)
                Sqlite.insertBy MetaDescriptorKey mdes >>= return . eitherOne
            )
        )
  where
    -- Returns True if #UNRELATED# is directly meta to the given key
    -- false if not or #UNRELATED# cannot be found in the db
    isMarkedUnrelated :: DescriptorAutoKey -> Result (Bool, Maybe DescriptorAutoKey)
    isMarkedUnrelated k = do
      unrelatedK <- lookupDescriptorAutoKey "#UNRELATED#"
      maybe
        (return (False, Nothing))
        ( \juk ->
            do
              relationCount <-
                Sqlite.count
                  ( k ==. InfraDescriptorIdField
                      &&. juk ==. MetaDescriptorIdField
                  )
              return (relationCount > 0, Just juk)
        )
        unrelatedK

deleteRelation :: MetaDescriptor -> Result ()
deleteRelation (MetaDescriptor m i) =
  Sqlite.delete (MetaDescriptorIdField ==. m &&. InfraDescriptorIdField ==. i)

lookupDescriptorAutoKey :: String -> Result (Maybe DescriptorAutoKey)
lookupDescriptorAutoKey =
  fmap head' . Sqlite.project Sqlite.AutoKeyField . Sqlite.like DescriptorField

getDescriptor :: DescriptorAutoKey -> Result (Maybe Descriptor)
getDescriptor = Sqlite.get

-- | Fetch all descriptors meta to the given key
fetchMetaDescriptors :: DescriptorAutoKey -> Result [DescriptorAutoKey]
fetchMetaDescriptors =
  Sqlite.project MetaDescriptorIdField . (==.) InfraDescriptorIdField

-- | Fetch all descriptors infra to the given key.
fetchInfraDescriptors :: DescriptorAutoKey -> Result [DescriptorAutoKey]
fetchInfraDescriptors =
  Sqlite.project InfraDescriptorIdField . (==.) MetaDescriptorIdField

-- | Fetch a tree of all descriptors infra to the given key.
-- Like a top-down hierarchy
fetchInfraTree :: DescriptorAutoKey -> Result DescriptorTree
fetchInfraTree k = do
  maybeKD <- getDescriptor k
  let parentTree = maybe NullTree Infra maybeKD
  childrenTrees <- (fetchInfraDescriptors >=> mapM fetchInfraTree) k
  return . Data.List.foldl' insertIntoDescriptorTree parentTree $ childrenTrees

-- | Fetch a tree off all descriptors meta to the given key.
-- Inverse of an infra tree, a bottom-up tree
fetchMetaTree :: DescriptorAutoKey -> Result DescriptorTree
fetchMetaTree k = do
  maybeKD <- getDescriptor k
  let parentTree = maybe NullTree Infra maybeKD
  childrenTrees <- (fetchMetaDescriptors >=> mapM fetchMetaTree) k
  return . Data.List.foldl' insertIntoDescriptorTree parentTree $ childrenTrees

-- | Fetches a flattened tree of all descriptors that are infra to #ALL#
-- if #ALL# is not present in DB then empty list is returned
fetchAllIndexedDescriptors :: Result [Descriptor]
fetchAllIndexedDescriptors = do
  allDescriptor <- lookupDescriptorAutoKey "#ALL#"
  maybe (return []) (fmap flattenTree . fetchInfraTree) allDescriptor

fetchFilesWithTag :: DescriptorAutoKey -> Result [FileAutoKey]
fetchFilesWithTag = Sqlite.project FileTagIdField . (==.) DescriptorTagIdField

fetchFilesInTree :: DescriptorTree -> Result [FileAutoKey]
fetchFilesInTree tr = do
  let descriptorList = flattenTree tr
  let mapDsTreeBackToDak tr = mapM (lookupDescriptorAutoKey . descriptor) tr
  dakList <- fmap catMaybes . mapDsTreeBackToDak $ descriptorList
  fileResults <- mapM fetchFilesWithTag dakList
  (return . concat) fileResults

fetchAllTags :: Result [((), Tag)]
fetchAllTags = Sqlite.selectAll

fetchTaggedFiles :: Result [FileAutoKey]
fetchTaggedFiles = fetchAllTags >>= return . map (fileTagId . snd)

fetchUntaggedFiles :: Result [FileAutoKey]
fetchUntaggedFiles =
  fetchTaggedFiles
    >>= Sqlite.project Sqlite.AutoKeyField
      . Groundhog.Sql.Functions.notIn_ Sqlite.AutoKeyField

fetchAllFiles :: Result [FileAutoKey]
fetchAllFiles = fetchAllFilesAndKeys >>= return . map fst

fetchAllFilesAndKeys :: Result [(Sqlite.AutoKey File, File)]
fetchAllFilesAndKeys = Sqlite.selectAll

fetchGetFile :: [FileAutoKey] -> Result [File]
fetchGetFile = fmap catMaybes . mapM getFile

getFile :: FileAutoKey -> Result (Maybe File)
getFile = Sqlite.get

fetchFileDescriptors :: FileAutoKey -> Result [DescriptorAutoKey]
fetchFileDescriptors = Sqlite.project DescriptorTagIdField . (==.) FileTagIdField

-- | If file exists in db return Just FileWithTags even if there are no tags
getFileWithTags :: FileAutoKey -> Result (Maybe FileWithTags)
getFileWithTags fk = do
  maybeFile <- getFile fk
  tagKeys <- fetchFileDescriptors fk
  tags <- fmap catMaybes . mapM Sqlite.get $ tagKeys
  maybe (return Nothing) (return . Just . flip FileWithTags tags) maybeFile

-- | Creates a single connection to the specified database
-- then runs the given Result monad.
connectThenRun ::
  ( Control.MonadBaseControl
      IO
      m,
    Control.Monad.IO.Class.MonadIO m
  ) =>
  String ->
  Result a ->
  m a
connectThenRun s a =
  Sqlite.withSqliteConn
    s
    ( Sqlite.runDbConn
        ( do
            Sqlite.runMigration createDatabaseMigration
            a
        )
    )

createDatabaseMigration ::
  StateT.StateT Groundhog.NamedMigrations (ReaderT.ReaderT Sqlite.Sqlite IO) ()
createDatabaseMigration = do
  Sqlite.migrate (undefined :: File)
  Sqlite.migrate (undefined :: Descriptor)
  Sqlite.migrate (undefined :: Tag)
  Sqlite.migrate (undefined :: MetaDescriptor)
