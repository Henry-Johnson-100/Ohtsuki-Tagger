{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.Task where

import Control.Applicative (Alternative (empty))
import Control.Monad (unless, (<=<), (>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.HashSet as HashSet
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Database.Tagger.Access
  ( DescriptorKey,
    FileKey,
    addDescriptor,
    addFile,
    addRepresentative,
    collectFileWithTagsByFileKey,
    deleteDatabaseFile,
    deleteDatabaseSubTags,
    deleteDescriptor,
    derefDatabaseFileWithTags,
    fetchInfraTree,
    fetchMetaDescriptors,
    fromDatabaseFileWithTags,
    getRepresentative,
    getsDatabaseTagIds,
    getsExclusiveInfraDescriptorKeys,
    getsUntaggedFileWithTags,
    hoistMaybe,
    insertDatabaseTag,
    lookupDescriptorPattern,
    lookupFileWithTagsByFileId',
    lookupFilesHavingDescriptorPattern,
    lookupFilesHavingFilePattern,
    lookupFilesHavingInfraTagRelationship,
    lookupFilesHavingSubTagRelationship,
    lookupTagLike,
    relate,
    renameDatabaseFile,
    renameDescriptor,
    uniqueDatabaseFileExists,
    unrelate,
    updateRepresentativeText,
    updateTagSubTagOfId,
  )
import Database.Tagger.Type
  ( Descriptor (descriptorId),
    DescriptorTree (NullTree),
    File (File, fileId),
    FileWithTags (FileWithTags, file),
    MetaDescriptor (MetaDescriptor),
    Representative (Representative),
    Tag (tagId),
    TagKey,
    TagPtr (Tag_),
    fwtFileEqual,
    getNode,
    getPathsToAdd,
    getTagPtr,
    tagNoId,
    tagPtrNoId,
  )
import Event.Parser
  ( ParseError,
    PseudoDescriptor (..),
    PseudoSubTag,
    QueryCriteriaLiteral (CLiteral, CNoLiteral),
    QuerySection (QuerySection, sectionContents),
    QueryToken (QueryToken, tokenCriteria),
    SetArithmeticLiteral (ALiteral, ANoLiteral),
    SubList (SubList),
    parseQuerySections,
    pseudoDescriptorText,
  )
import IO
  ( CreateProcess (delegate_ctlc, new_session),
    Exception (eLabel, liftEx),
    createProcess,
    deleteFileSystemFile,
    dropFileName,
    getConfigPath,
    guardDirectoryExists,
    guardException,
    guardFileDoesNotExist,
    guardFileExists,
    hGetContents,
    hPrint,
    hPutStrLn,
    proc,
    renameFileSystemFile,
    stderr,
    stdout,
    waitForProcess,
  )
import qualified Toml
import Type.BufferList (BufferList, Cycleable (cFromList))
import Type.Config (TaggerConfig, taggerConfigCodec)
import Type.Model
  ( FileSetArithmetic (..),
    Intersectable (diffBy, intersectBy, unionBy),
    QueryCriteria (..),
  )
import Util.Core (head', tail')

type ConnString = String

newtype TaskException = TaskException String deriving (Eq)

instance Show TaskException where
  show (TaskException msg) = "Task exception: " ++ msg

instance Exception TaskException where
  liftEx = TaskException
  eLabel f (TaskException m) = f m

runQuery ::
  Connection ->
  FileSetArithmetic ->
  QueryCriteria ->
  BufferList FileWithTags ->
  T.Text ->
  IO
    (BufferList FileWithTags)
runQuery c a qc currentSelection t =
  case parseQuerySections t of
    Left ex -> do
      hPrint stderr ex
      empty
    Right r -> queryWithParseResults c a qc currentSelection r

queryWithParseResults ::
  Traversable t =>
  Connection ->
  FileSetArithmetic ->
  QueryCriteria ->
  BufferList FileWithTags ->
  [QuerySection (t (SubList (QueryToken PseudoDescriptor)))] ->
  IO (BufferList FileWithTags)
queryWithParseResults c a ByUntagged currentBuffer _ = do
  byUntaggedResults <-
    queryWithQueryTokenLiteral
      c
      ByUntagged
      (QueryToken (CLiteral ByUntagged) (PDescriptor "Untagged Search"))
  let untaggedQuerySections = QuerySection (ALiteral a) byUntaggedResults
  return . bfComb a currentBuffer . cFromList . sectionContents $ untaggedQuerySections
queryWithParseResults c a qc currentBuffer qss = do
  queriedSections <- mapM (queryWithQuerySection c qc) qss
  let combinedResults = combineQueriedSection a queriedSections
  return . bfComb a currentBuffer . cFromList . sectionContents $ combinedResults

bfComb ::
  Intersectable l =>
  FileSetArithmetic ->
  l FileWithTags ->
  l FileWithTags ->
  l FileWithTags
bfComb a bf bxs =
  case a of
    Union -> unionBy fwtFileEqual bf bxs
    Intersect -> intersectBy fwtFileEqual bf bxs
    Diff -> diffBy fwtFileEqual bf bxs

combineQueriedSection ::
  FileSetArithmetic ->
  [QuerySection [FileWithTags]] ->
  QuerySection [FileWithTags]
combineQueriedSection a [] = QuerySection (ALiteral a) []
combineQueriedSection a qqs = L.foldl1' (combine a) qqs
  where
    combine ::
      FileSetArithmetic ->
      QuerySection [FileWithTags] ->
      QuerySection [FileWithTags] ->
      QuerySection [FileWithTags]
    combine setArithmetic (QuerySection _ sxs) (QuerySection ay sys) =
      case ay of
        ANoLiteral -> combineIntersectable setArithmetic ay sxs sys
        ALiteral fromLiteralSetArithmetic ->
          combineIntersectable fromLiteralSetArithmetic ay sxs sys
      where
        combineIntersectable a'' aTo xs'' ys'' = QuerySection aTo $
          case a'' of
            Union -> unionBy fwtFileEqual xs'' ys''
            Intersect -> intersectBy fwtFileEqual xs'' ys''
            Diff -> diffBy fwtFileEqual xs'' ys''

queryWithQuerySection ::
  Traversable t =>
  Connection ->
  QueryCriteria ->
  QuerySection (t (SubList (QueryToken PseudoDescriptor))) ->
  IO (QuerySection [FileWithTags])
queryWithQuerySection c qc qs@(QuerySection _ ss) = do
  ss' <- fmap concat . mapM (queryWithQueryTokenSubList c qc) $ ss
  return $ qs {sectionContents = ss'}

queryWithQueryTokenSubList ::
  Connection ->
  QueryCriteria ->
  SubList (QueryToken PseudoDescriptor) ->
  IO [FileWithTags]
queryWithQueryTokenSubList c qc (SubList h []) = queryWithQueryTokenLiteral c qc h
queryWithQueryTokenSubList c qc (SubList h ts) = do
  fks <- fmap concat . mapM (queryWithSingleSubTagQuery h) $ ts
  dbfwts <- collectFileWithTagsByFileKey c fks
  fmap catMaybes . mapM (runMaybeT . derefDatabaseFileWithTags c) $ dbfwts
  where
    queryWithSingleSubTagQuery ::
      QueryToken PseudoDescriptor -> QueryToken PseudoDescriptor -> IO [FileKey]
    queryWithSingleSubTagQuery
      qtH@(QueryToken _ (PDescriptor _))
      qtT@(QueryToken _ (PDescriptor _)) = do
        headKeys <- getDescriptorKeysFromLiteralPattern c qc qtH
        tailKeys <- getDescriptorKeysFromLiteralPattern c qc qtT
        fmap concat
          . mapM (flip (lookupFilesHavingSubTagRelationship c) tailKeys)
          $ headKeys

    getDescriptorKeysFromLiteralPattern ::
      Connection ->
      QueryCriteria ->
      QueryToken PseudoDescriptor ->
      IO [DescriptorKey]
    getDescriptorKeysFromLiteralPattern c' qc' qt@(QueryToken CNoLiteral _) =
      getDescriptorKeysFromLiteralPattern c' qc' (qt {tokenCriteria = CLiteral qc})
    getDescriptorKeysFromLiteralPattern
      c'
      _
      (QueryToken (CLiteral crit) (PDescriptor pd)) =
        case crit of
          ByTag -> map descriptorId <$> lookupDescriptorPattern c' pd
          ByRelation -> do
            dks <- map descriptorId <$> lookupDescriptorPattern c' pd
            idks <- L.foldl1' L.union <$> mapM (getsExclusiveInfraDescriptorKeys c') dks
            return $ L.union dks idks
          ByPattern -> return []
          ByUntagged -> return []

queryWithQueryTokenLiteral ::
  Connection ->
  QueryCriteria ->
  QueryToken PseudoDescriptor ->
  IO [FileWithTags]
queryWithQueryTokenLiteral c _ (QueryToken (CLiteral crit) (PDescriptor pd)) =
  case crit of
    ByTag -> queryByDescriptorLiteral pd
    ByRelation -> queryByRelationLiteral pd
    ByPattern -> queryByFilePatternLiteral pd
    ByUntagged -> returnUntaggedFiles
  where
    queryByDescriptorLiteral :: T.Text -> IO [FileWithTags]
    queryByDescriptorLiteral t = do
      fks <- lookupFilesHavingDescriptorPattern c (PDescriptor t)
      dbfwts <- collectFileWithTagsByFileKey c fks
      fmap catMaybes . mapM (runMaybeT . derefDatabaseFileWithTags c) $ dbfwts
    queryByRelationLiteral :: T.Text -> IO [FileWithTags]
    queryByRelationLiteral t = do
      -- Can actually just be a mapM honestly
      md <- fmap head' . lookupDescriptorPattern c $ t
      maybe
        (return [])
        ( \d -> do
            fks <- lookupFilesHavingInfraTagRelationship c . descriptorId $ d
            dbfwts <- collectFileWithTagsByFileKey c fks
            fmap catMaybes . mapM (runMaybeT . derefDatabaseFileWithTags c) $ dbfwts
        )
        md
    queryByFilePatternLiteral :: T.Text -> IO [FileWithTags]
    queryByFilePatternLiteral t = do
      fks <- lookupFilesHavingFilePattern c t
      dbfwts <- collectFileWithTagsByFileKey c fks
      fmap catMaybes . mapM (runMaybeT . derefDatabaseFileWithTags c) $ dbfwts
    returnUntaggedFiles :: IO [FileWithTags]
    returnUntaggedFiles = getsUntaggedFileWithTags c
queryWithQueryTokenLiteral c qc (QueryToken CNoLiteral ps) =
  queryWithQueryTokenLiteral c qc (QueryToken (CLiteral qc) ps)

getRepresentative :: Connection -> Descriptor -> MaybeT IO Representative
getRepresentative c = Database.Tagger.Access.getRepresentative c . descriptorId

createRepresentative ::
  Connection -> File -> Descriptor -> Maybe T.Text -> IO ()
createRepresentative c f d des =
  addRepresentative c $ Representative f d des

updateRepresentativeText :: Connection -> Descriptor -> T.Text -> IO ()
updateRepresentativeText c =
  Database.Tagger.Access.updateRepresentativeText c . descriptorId

renameDescriptor :: Connection -> Descriptor -> T.Text -> IO ()
renameDescriptor = Database.Tagger.Access.renameDescriptor

runShellCmds :: [String] -> [String] -> IO ()
runShellCmds cs fwtString = do
  let rawCmd = L.break (L.isInfixOf ";") cs
  unless (null . fst $ rawCmd) $ do
    let prog = head . fst $ rawCmd
        args = findReplaceFileArg (tail' . fst $ rawCmd) fwtString
    runShellCmd prog args
    flip runShellCmds fwtString . tail' . snd $ rawCmd
  where
    findReplaceFileArg args files =
      let !atFileArg = L.break (L.isInfixOf "%file") args
       in fst atFileArg ++ files ++ (tail' . snd $ atFileArg)

    runShellCmd :: String -> [String] -> IO ()
    runShellCmd c args = do
      let cmd =
            (proc c args)
              { delegate_ctlc = True,
                new_session = True
              }
      p <- createProcess cmd
      let pout = (\(_, h, _, _) -> h) p
          perr = (\(_, _, h, _) -> h) p
          pProc = (\(_, _, _, p') -> p') p
      hReadMaybe stdout pout
      hReadMaybe stderr perr
      hPrint stderr <=< waitForProcess $ pProc
      where
        hReadMaybe oh mh =
          maybe (pure ()) (hGetContents >=> hPutStrLn oh) mh

exportConfig :: TaggerConfig -> IO ()
exportConfig tc = do
  configPath <- getConfigPath
  encodingMsg <- Toml.encodeToFile taggerConfigCodec configPath tc
  putStrLn . T.unpack $ encodingMsg

addPath :: Connection -> T.Text -> IO [FileWithTags]
addPath c p = do
  pathsToAdd <- getPathsToAdd p
  addedFiles <- mapM (addFile c) pathsToAdd
  return $ flip FileWithTags HashSet.empty <$> addedFiles

associateTag :: Connection -> Tag -> Tag -> IO ()
associateTag c tWith t = updateTagSubTagOfId c (getTagPtr t) (tagId tWith)

tag :: Connection -> [FileWithTags] -> Either ParseError [PseudoSubTag] -> IO ()
tag c fwts =
  either
    (hPrint stderr)
    ( \psts -> do
        let newTagTuples = (,) <$> fwts <*> psts
        mapM_ (runMaybeT . uncurry tag') newTagTuples
    )
  where
    tag' :: FileWithTags -> PseudoSubTag -> MaybeT IO ()
    tag' fwt' pst' = do
      d <-
        hoistMaybe . head'
          <=< lift . lookupDescriptorPattern c . pseudoDescriptorText . fst
          $ pst'
      subDescriptors <-
        lift
          . fmap concat
          . mapM (lookupDescriptorPattern c . pseudoDescriptorText)
          . snd
          $ pst'
      let fileId' = fileId . file $ fwt'
          mainTag = tagPtrNoId fileId' (descriptorId d) Nothing
      mainTagKey <- lift $ do
        maybeExists <- runMaybeT . lookupTagLike c $ tagNoId (file fwt') d Nothing
        maybe (insertDatabaseTag c mainTag) (pure . (\(Tag_ tk _ _ _) -> tk)) maybeExists
      tag'' fileId' mainTagKey subDescriptors

    tag'' :: FileKey -> TagKey -> [Descriptor] -> MaybeT IO ()
    tag'' fileId' mainTagKey' subDescriptors' = do
      let subTags =
            tagPtrNoId fileId'
              <$> map descriptorId subDescriptors' <*> [Just mainTagKey']
      lift . mapM_ (insertDatabaseTag c) $ subTags

getRefreshedFWTs :: Connection -> [FileWithTags] -> IO [FileWithTags]
getRefreshedFWTs c fwts = do
  let fids = map (fileId . file) fwts
  refreshedDbFwts <- fmap concat . mapM (lookupFileWithTagsByFileId' c) $ fids
  fmap catMaybes . mapM (runMaybeT . fromDatabaseFileWithTags c) $ refreshedDbFwts

untag :: Connection -> [FileWithTags] -> Either ParseError [PseudoSubTag] -> IO ()
untag c fwts =
  either
    (hPrint stderr)
    ( \psts -> do
        let toDeleteTuples = (,) <$> fwts <*> psts
        mapM_ (runMaybeT . uncurry untag') toDeleteTuples
    )
  where
    untag' :: FileWithTags -> PseudoSubTag -> MaybeT IO ()
    untag' fwt' pst' = do
      des <-
        hoistMaybe . head'
          <=< lift . lookupDescriptorPattern c . pseudoDescriptorText . fst
          $ pst'
      subDes <-
        lift
          . fmap concat
          . mapM (lookupDescriptorPattern c . pseudoDescriptorText)
          . snd
          $ pst'
      let dbTags' =
            tagPtrNoId (fileId . file $ fwt') (descriptorId des)
              <$> ( Nothing :
                    map (Just . descriptorId) subDes
                  )
      dbTags <- lift $ getsDatabaseTagIds c dbTags'
      lift . deleteDatabaseSubTags c $ dbTags

untagWithTag :: Connection -> [Tag] -> IO ()
untagWithTag c = deleteDatabaseSubTags c . map getTagPtr

relateTo :: Connection -> [Descriptor] -> [Descriptor] -> IO ()
relateTo c m i = do
  let metaDescriptors = MetaDescriptor <$> (descriptorId <$> m) <*> (descriptorId <$> i)
  _x <- runMaybeT . mapM_ (relate c) $ metaDescriptors
  return ()

unrelate :: Connection -> [Descriptor] -> IO ()
unrelate c i = do
  _x <- runMaybeT . mapM_ (Database.Tagger.Access.unrelate c . descriptorId) $ i
  return ()

lookupInfraDescriptorTree ::
  Connection -> T.Text -> IO DescriptorTree
lookupInfraDescriptorTree c dT = do
  result <-
    runMaybeT
      ( do
          d <-
            hoistMaybe . head'
              <=< lift . lookupDescriptorPattern c
              $ dT
          fetchInfraTree c . descriptorId $ d
      )
  return . fromMaybe NullTree $ result

getALLInfraTree ::
  Connection -> IO DescriptorTree
getALLInfraTree c = do
  result <-
    runMaybeT
      ( do
          allDescriptor <-
            hoistMaybe . head'
              <=< lift
                . lookupDescriptorPattern c
              $ "#ALL#"
          fetchInfraTree c . descriptorId $ allDescriptor
      )
  return . fromMaybe NullTree $ result

getParentDescriptorTree ::
  Connection ->
  DescriptorTree ->
  IO DescriptorTree
getParentDescriptorTree c tr = do
  result <-
    runMaybeT
      ( do
          headNode <- hoistMaybe . getNode $ tr
          firstMetaDescriptor <-
            hoistMaybe . head'
              <=< ( lift
                      . fetchMetaDescriptors c
                      . descriptorId
                  )
              $ headNode
          fetchInfraTree c
            . descriptorId
            $ firstMetaDescriptor
      )
  maybe (getALLInfraTree c) return result

createNewDescriptors :: Connection -> [T.Text] -> IO ()
createNewDescriptors c ts = do
  _x <- mapM (runMaybeT . addDescriptor c) ts
  return ()

deleteDescriptor :: Connection -> Descriptor -> IO ()
deleteDescriptor = Database.Tagger.Access.deleteDescriptor

renameTaggerFile :: Connection -> File -> T.Text -> ExceptT TaskException IO ()
renameTaggerFile c f@(File fk p) to = do
  let pPath = T.unpack p
      toPath = T.unpack to
  guardException ("Unique file, " ++ show f ++ " Not found in the database.")
    <=< lift . uniqueDatabaseFileExists c
    $ f
  guardFileExists pPath
  guardFileDoesNotExist toPath
  guardDirectoryExists . dropFileName $ toPath
  lift $ renameFileSystemFile p to
  lift $ renameDatabaseFile c fk to

-- -- |
-- -- A special case of renameTaggerFile that keeps the path but
-- -- changes just the file basename.
-- renameTaggerFileBaseName :: Connection -> File -> T.Text -> ExceptT TaskException IO ()
-- renameTaggerFileBaseName c f@(File _ p) to = do
--   let fileDir = dropFileName . T.unpack $ p
--   renameTaggerFile c f (fileDir <~> to)

-- |
-- A special case of renameTaggerFile that preserves the basename but moves the path.
moveTaggerFile :: Connection -> File -> T.Text -> ExceptT TaskException IO ()
moveTaggerFile c f to = undefined

deleteTaggerFile :: Connection -> File -> ExceptT TaskException IO ()
deleteTaggerFile c f@(File fk p) = do
  guardException ("Unique file, " ++ show f ++ " not found in the database.")
    <=< lift . uniqueDatabaseFileExists c
    $ f
  guardFileExists . T.unpack $ p
  lift $ deleteFileSystemFile p
  lift $ deleteDatabaseFile c fk

-- |
-- Like deleteTaggerFile but only removes the file from the database
-- and NOT the filesystem.
--
-- Does not check if a file exists in the filesystem,
-- but only checks for a unique file in the database.
removeTaggerFile :: Connection -> File -> ExceptT TaskException IO ()
removeTaggerFile c f@(File fk _) = do
  guardException ("Unique file, " ++ show f ++ " not found in the database")
    <=< lift . uniqueDatabaseFileExists c
    $ f
  lift $ deleteDatabaseFile c fk