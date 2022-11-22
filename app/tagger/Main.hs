{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import CLI.Data
import Control.Lens ((&), (.~), (^.))
import Control.Monad (filterM, void, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (
  runExceptT,
 )
import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
  ask,
  asks,
 )
import Control.Monad.Trans.State.Strict (
  StateT,
  execStateT,
  modify,
 )
import qualified Data.Foldable as F
import Data.Functor (($>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.IntMap as IM
import Data.List (sortOn)
import Data.Model.Core (
  createTaggerModel,
  focusedFileDefaultDataFile,
 )
import qualified Data.OccurrenceMap as OM
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Version (showVersion)
import Database.Tagger (
  Descriptor (..),
  File (..),
  allDescriptors,
  allFiles,
  allTags,
  close,
  connName,
  deleteFiles,
  getAllInfra,
  getTagOccurrencesByDescriptorKeys,
  mvFile,
  open,
  openOrCreate,
  queryForFileByPattern,
  rmFile,
 )
import Database.Tagger.Type (TaggedConnection)
import Interface (runTagger)
import Options.Applicative (
  Alternative (some, (<|>)),
  ParserInfo,
  argument,
  execParser,
  flag',
  help,
  helper,
  idm,
  info,
  long,
  metavar,
  short,
  str,
  switch,
 )
import Paths_tagger (getDataFileName)
import System.Directory (
  doesFileExist,
  getCurrentDirectory,
  makeAbsolute,
  setCurrentDirectory,
 )
import System.FilePath (makeRelative, takeDirectory)
import System.IO (hPutStrLn, stderr)
import Tagger.Info (taggerVersion)
import Text.TaggerQL (runQuery, tagFile)
import Util (addFiles)

main :: IO ()
main = do
  args <- execParser programParser
  mainProgram args

programParser :: ParserInfo Program
programParser = info (helper <*> (versionParser <|> withDBParser)) idm
 where
  versionParser =
    switch (short 'v' <> long "version" <> help "Show the version.")
      $> Version
  withDBParser =
    WithDB
      <$> argument
        str
        (metavar "DATABASE" <> help "Path to the tagger database file.")
      <*> ( flag' Create (short 'c' <> long "create" <> help "Create database")
              <|> ( flag'
                      Query
                      ( short 'q' <> long "query"
                          <> help "Run the given TaggerQL on the database."
                      )
                      <*> argument str (metavar "QUERY")
                      <*> switch
                        ( long "relative"
                            <> help "Output query results with relative paths."
                        )
                  )
              <|> flag' Stats (long "stats" <> help "Show stats for the given database.")
              <|> flag' Audit (long "audit" <> help "Run audit on the given database.")
              <|> ( flag'
                      Add
                      ( short 'a'
                          <> long "add"
                          <> help "Add file(s) at the given path to the database."
                      )
                      <*> some (argument str (metavar "PATHS"))
                  )
              <|> ( flag'
                      Tag
                      ( long "tag"
                          <> help
                            "Run a tagging expression on the file \
                            \matching the given pattern."
                      )
                      <*> argument str (metavar "PATH_PATTERN")
                      <*> argument str (metavar "EXPR")
                  )
              <|> ( flag'
                      Move
                      ( long "move"
                          <> help
                            "Rename or move a file that matches the given pattern, \
                            \both in the database and in the filesystem.\
                            \ Does nothing if the pattern matches 0 or many files."
                      )
                      <*> argument str (metavar "FROM")
                      <*> argument str (metavar "TO")
                  )
              <|> ( flag'
                      Remove
                      ( long "REMOVE"
                          <> help
                            "Remove file(s) matching the given pattern\
                            \ from the database."
                      )
                      <*> some (argument str (metavar "PATTERN"))
                  )
              <|> ( flag'
                      Delete
                      ( long "DELETE"
                          <> help
                            "Deletes file(s) matching the given pattern\
                            \ from the database AND filesystem!"
                      )
                      <*> some (argument str (metavar "PATTERN"))
                  )
              <|> pure Default
          )

data Program
  = Version
  | WithDB FilePath Command
  deriving (Show, Eq)

data Command
  = Default
  | Create
  | Add [FilePath]
  | Move FilePath FilePath
  | Remove [FilePath]
  | Delete [FilePath]
  | Stats
  | Audit
  | Query String Bool
  | Tag FilePath String
  | Info [FilePath]
  deriving (Show, Eq)

mainProgram :: Program -> IO ()
mainProgram Version = putStrLn . showVersion $ taggerVersion
mainProgram (WithDB dbPath Create) = do
  c <- openOrCreate dbPath
  close c
mainProgram (WithDB dbPath cm) = do
  curDir <- getCurrentDirectory
  dbDir <- makeAbsolute . takeDirectory $ dbPath
  setCurrentDirectory dbDir
  ec <- runExceptT $ open dbPath
  flip (either (T.IO.hPutStrLn stderr)) ec $ \c -> do
    case cm of
      Default -> do
        defaultFile <- T.pack <$> getDataFileName focusedFileDefaultDataFile
        runTagger
          ( createTaggerModel
              c
              (Descriptor (-1) "fake descriptor")
              (Descriptor (-2) "fake #UNRELATED#")
              defaultFile
          )
      Add ss -> mapM_ (addFiles c . T.pack) ss
      Move s toN -> do
        fs <- queryForFileByPattern (T.pack s) c
        case fs of
          [f] -> do
            mvFile c (fileId f) (T.pack toN)
          [] -> hPutStrLn stderr $ "No files in database matching: '" <> s <> "'"
          _tooManyResults ->
            hPutStrLn stderr $
              "Too many files matching '"
                <> s
                <> "' only one file rename is permitted at a time."
      Remove ss -> do
        fs <- concat <$> mapM ((`queryForFileByPattern` c) . T.pack) ss
        deleteFiles (fileId <$> fs) c
      Delete ss -> do
        fs <- concat <$> mapM ((`queryForFileByPattern` c) . T.pack) ss
        mapM_ (rmFile c . fileId) fs
      Stats -> runReaderT showStats c
      Audit -> runReaderT mainReportAudit c
      Query (T.pack -> q) rel -> do
        let (T.unpack -> connPath) = c ^. connName
        eQueryResults <- runExceptT $ runQuery c q
        either
          (mapM_ T.IO.putStrLn)
          ( \queryResults ->
              if HS.null queryResults
                then T.IO.hPutStrLn stderr "No Results."
                else
                  mapM_
                    ( ( T.IO.putStrLn . T.pack
                          <=< if rel
                            then pure
                            else makeAbsolute
                      )
                        . makeRelative connPath
                        . T.unpack
                        . filePath
                    )
                    . sortOn filePath
                    . F.toList
                    $ queryResults
          )
          eQueryResults
      Tag s (T.pack -> tExpr) -> do
        fs <- queryForFileByPattern (T.pack s) c
        mapM_ ((\fk -> tagFile fk c tExpr) . fileId) fs
      Info _ -> error "not implemented yet"
      _alreadHandled -> pure ()
  setCurrentDirectory curDir

showStats :: ReaderT TaggedConnection IO ()
showStats = do
  connPath <- asks (^. connName)
  (TaggerDBStats fc dc tc) <- getStats
  liftIO $ do
    T.IO.putStrLn $ "The database, " <> connPath <> ", has:"
    T.IO.putStrLn $ (T.pack . show $ fc) <> " files"
    T.IO.putStrLn $ (T.pack . show $ dc) <> " descriptors"
    T.IO.putStrLn $ (T.pack . show $ tc) <> " applied tags"
 where
  getStats :: ReaderT TaggedConnection IO TaggerDBStats
  getStats = do
    tc <- ask
    liftIO $
      TaggerDBStats
        <$> (length <$> allFiles tc)
        <*> (length <$> allDescriptors tc)
        <*> (length <$> allTags tc)

mainReportAudit :: ReaderT TaggedConnection IO ()
mainReportAudit = do
  tc <- ask
  let dbText = tc ^. connName
  void $
    do
      liftIO . T.IO.putStrLn $ "Running audit on: " <> dbText
      auditResult <- auditDatabase
      liftIO $ reportAudit auditResult
 where
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
   where
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
