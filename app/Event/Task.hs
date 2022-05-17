{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Event.Task where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Char
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.Tagger.Access
import Database.Tagger.Type
import Event.Parser
import IO
import qualified Toml
import Type.BufferList
import Type.Config
import Type.Model
import Util.Core

type ConnString = String

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
queryWithParseResults c a qc currentBuffer qss = do
  queriedSections <- mapM (queryWithQuerySection c qc) qss
  let combinedResults = combineQueriedSection a queriedSections
  return . bfComb a currentBuffer . cFromList . sectionContents $ combinedResults
  where
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
    combine a (QuerySection ax sxs) (QuerySection ay sys) =
      case ay of
        ANoLiteral -> combine'' a ay sxs sys
        ALiteral a' -> combine'' a' ay sxs sys
      where
        combine'' a'' aTo xs'' ys'' = QuerySection aTo $
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
queryWithQuerySection c qc qs@(QuerySection a ss) = do
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
      qtH@(QueryToken ctH pdH@(PDescriptor dH))
      qtT@(QueryToken ctT pdT@(PDescriptor dT)) = do
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
    getDescriptorKeysFromLiteralPattern c qc qt@(QueryToken CNoLiteral _) =
      getDescriptorKeysFromLiteralPattern c qc (qt {tokenCriteria = CLiteral qc})
    getDescriptorKeysFromLiteralPattern
      c
      qc
      qt@(QueryToken (CLiteral crit) ps@(PDescriptor pd)) =
        case crit of
          ByTag -> map descriptorId <$> lookupDescriptorPattern c pd
          ByRelation -> do
            dks <- map descriptorId <$> lookupDescriptorPattern c pd
            idks <- L.foldl1' L.union <$> mapM (getsExclusiveInfraDescriptorKeys c) dks
            return $ L.union dks idks
          ByPattern -> return []
          ByUntagged -> return []

queryWithQueryTokenLiteral ::
  Connection ->
  QueryCriteria ->
  QueryToken PseudoDescriptor ->
  IO [FileWithTags]
queryWithQueryTokenLiteral c qc (QueryToken (CLiteral crit) ps@(PDescriptor pd)) =
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

shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle xs = do
  g <- initStdGen
  let shuffled = shuffle' xs (length xs) g
  return shuffled

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
      print <=< waitForProcess $ pProc
      where
        hReadMaybe oh mh =
          maybe (pure ()) (hGetContents >=> hPutStrLn oh) mh
        hCloseMaybe = maybe (pure ()) hClose

exportConfig :: TaggerConfig -> IO ()
exportConfig tc = do
  configPath <- getConfigPath
  encodingMsg <- Toml.encodeToFile taggerConfigCodec configPath tc
  putStrLn . T.unpack $ encodingMsg

addPath :: Connection -> T.Text -> IO [FileWithTags]
addPath c p = do
  pathsToAdd <- getPathsToAdd p
  addedFiles <- mapM (addFile c) pathsToAdd
  return $ FileWithTags <$> addedFiles <*> []

associateTag :: Connection -> Tag -> Tag -> IO ()
associateTag c tWith t = updateTagSubTagOfId c (getTagPtr t) (tagId tWith)

tag :: Connection -> [FileWithTags] -> Either ParseError [PseudoSubTag] -> IO ()
tag c fwts =
  either
    (hPrint stderr)
    ( \psts -> do
        let newTagTuples = (,) <$> fwts <*> psts
        tags <- mapM (runMaybeT . uncurry tag') newTagTuples
        return ()
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
        deletedTags <- mapM (runMaybeT . uncurry untag') toDeleteTuples
        return ()
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

doQueryWithCriteria ::
  QueryCriteria ->
  Connection ->
  T.Text ->
  IO [FileWithTags]
doQueryWithCriteria qc =
  case qc of
    ByTag -> \c t -> queryByTag c (parseQuery t)
    ByRelation -> \c t -> queryRelation c (T.words t)
    ByUntagged -> \c t -> queryUntagged c (T.words t)
    ByPattern -> \c t -> queryFilePattern c (T.words t)
  where
    queryByTag ::
      Connection ->
      Either ParseError [PseudoSubTag] ->
      IO [FileWithTags]
    queryByTag c =
      either
        (\e -> hPrint stderr e >> return [])
        (fmap concat . mapM queryByPseudoSubTag)
      where
        queryByPseudoSubTag :: PseudoSubTag -> IO [FileWithTags]
        queryByPseudoSubTag (pd, []) = do
          dbFwts <- lookupFileWithTagsByDescriptorPattern c . pseudoDescriptorText $ pd
          fmap catMaybes . mapM (runMaybeT . fromDatabaseFileWithTags c) $ dbFwts
        queryByPseudoSubTag (pd, spds) = do
          let mainDes = pseudoDescriptorText pd
              subDess = map pseudoDescriptorText spds
          dbFwts <-
            fmap concat
              . mapM (lookupFileWithTagsBySubTagDescriptorText c mainDes)
              $ subDess
          fmap catMaybes . mapM (runMaybeT . fromDatabaseFileWithTags c) $ dbFwts
    queryRelation ::
      Connection -> [T.Text] -> IO [FileWithTags]
    queryRelation c rs = do
      ds <- fmap concat . mapM (lookupDescriptorPattern c) $ rs
      dbfwts <-
        fmap concat
          . mapM
            ( lookupFileWithTagsByInfraRelation c
                . descriptorId
            )
          $ ds
      fmap catMaybes . mapM (runMaybeT . fromDatabaseFileWithTags c) $ dbfwts
    queryUntagged ::
      Connection -> [T.Text] -> IO [FileWithTags]
    queryUntagged c ns = do
      untaggedFwts <- getsUntaggedFileWithTags c
      case ns of
        [] -> return untaggedFwts
        (n : _) ->
          if T.all isDigit n
            then return . take (read . T.unpack $ n) $ untaggedFwts
            else return untaggedFwts
    queryFilePattern ::
      Connection -> [T.Text] -> IO [FileWithTags]
    queryFilePattern c ps = do
      dbFwts <- fmap concat . mapM (lookupFileWithTagsByFilePattern' c) $ ps
      fmap catMaybes . mapM (runMaybeT . fromDatabaseFileWithTags c) $ dbFwts

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

getUnrelatedInfraTree :: Connection -> IO DescriptorTree
getUnrelatedInfraTree c = do
  result <-
    runMaybeT
      ( do
          unrelatedDescriptor <-
            hoistMaybe . head' <=< lift . lookupDescriptorPattern c $ "#UNRELATED#"
          fetchInfraTree c . descriptorId $ unrelatedDescriptor
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
