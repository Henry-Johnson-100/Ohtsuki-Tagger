{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
import Type.Config
import Type.Model
import Util.Core

type ConnString = String

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
      let mainTagDescriptorId = descriptorId d
          fileId' = fileId . file $ fwt'
          mainTag = TagNoId_ $ Tag_ (-1) fileId' mainTagDescriptorId Nothing
      mainTagKey <- lift $ insertDatabaseTag c mainTag
      let subTags =
            TagNoId_
              <$> ( Tag_ (-1) fileId'
                      <$> map descriptorId subDescriptors <*> [Just mainTagKey]
                  )
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
            TagNoId_ . Tag_ (-1) (fileId . file $ fwt') (descriptorId des)
              <$> ( Nothing :
                    map (Just . descriptorId) subDes
                  )
      dbTags <- lift $ getsDatabaseTagIds c dbTags'
      lift . deleteDatabaseSubTags c $ dbTags

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
  [T.Text] ->
  IO [FileWithTags]
doQueryWithCriteria qc =
  case qc of
    ByTag -> queryByTag
    ByRelation -> queryRelation
    ByUntagged -> queryUntagged
    ByPattern -> queryFilePattern
  where
    queryByTag ::
      Connection ->
      [T.Text] ->
      IO [FileWithTags]
    queryByTag c ts = do
      dbFwts <- fmap concat . mapM (lookupFileWithTagsByTagPattern' c) $ ts
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
