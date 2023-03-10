{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import CLI.Data (
  HasMissingFiles (missingFiles),
  HasUnusedDescriptorTrees (unusedDescriptorTrees),
  TaggerDBAudit,
  TaggerDBStats (TaggerDBStats),
 )
import Control.Lens ((&), (.~), (^.))
import Control.Monad (filterM, void, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (
  runExceptT,
 )
import Control.Monad.Trans.Maybe (runMaybeT)
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
import qualified Data.HierarchyMap as HRM
import qualified Data.IntMap as IM
import Data.List (sortOn)
import qualified Data.List as L
import Data.Model.Core (
  createTaggerModel,
  focusedFileDefaultDataFile,
 )
import qualified Data.OccurrenceMap as OM
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Version (showVersion)
import Database.Tagger (
  ConcreteTag (ConcreteTag),
  ConcreteTaggedFile (ConcreteTaggedFile),
  Descriptor (..),
  File (fileId, filePath),
  HasConnName (connName),
  RecordKey,
  TaggedConnection,
  allDescriptors,
  allFiles,
  allTags,
  close,
  concreteTagDescriptor,
  deleteFiles,
  getAllInfra,
  getInfraChildren,
  getTagOccurrencesByDescriptorKeys,
  mvFile,
  open,
  openOrCreate,
  queryForConcreteTaggedFileWithFileId,
  queryForDescriptorByPattern,
  queryForFileByPattern,
  rmFile,
 )
import Interface (runTagger)
import Options.Applicative (
  Alternative (some, (<|>)),
  ParserInfo,
  argument,
  command,
  execParser,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  progDesc,
  short,
  str,
  subparser,
  switch,
 )
import Paths_tagger (getDataFileName)
import System.Directory (
  doesFileExist,
  makeAbsolute,
  withCurrentDirectory,
 )
import System.FilePath (makeRelative, takeDirectory)
import System.IO (hPutStrLn, stderr)
import Tagger.Info (taggerVersion)
import Text.TaggerQL.Expression.Engine (
  yuiQLCreateDescriptors,
  yuiQLFileQuery,
  yuiQLTagFile,
 )
import Util (addFiles, compareConcreteTags)

main :: IO ()
main = do
  args <- execParser programParser
  mainProgram args

programParser :: ParserInfo Program
programParser =
  info
    (helper <*> (versionParser <|> withDBParser))
    ( header
        "YUITAGGER: A GUI image-tagging program with a unique sub-tagging \
        \system and proprietary query language, YuiQL."
        <> progDesc
          "YuiTagger is, first and foremost, a GUI program.\
          \ Run tagger with a path to a database and no other arguments to start the GUI.\
          \ Additionally, there are some limited CLI operations exposed for convenience."
    )
 where
  versionParser =
    switch (short 'v' <> long "version" <> help "Show the version.")
      $> Version
  withDBParser =
    WithDB
      <$> argument
        str
        (metavar "DATABASE" <> help "Path to the tagger database file.")
      <*> ( subparser
              ( command "create" createParser
                  <> command "query" queryParser
                  <> command "stats" statisticsParser
                  <> command "audit" auditParser
                  <> command "describe" describeParser
                  <> command "add" addParser
                  <> command "system" systemParser
              )
              <|> pure Default
          )
   where
    createParser =
      info
        (pure Create)
        (progDesc "Create the database at the specified file path.")
    queryParser =
      info
        ( helper
            <*> ( Query
                    <$> ( UserSupplied
                            <$> argument
                              str
                              ( metavar "YuiQL"
                                  <> help
                                    "A YuiQL query for files. \
                                    \Reads from stdin if no query is given."
                              )
                            <|> pure GetFromStdIn
                        )
                    <*> switch
                      ( long "relative"
                          <> short 'r'
                          <> help "Output filepaths as they appear in the database."
                      )
                )
        )
        ( progDesc
            "Run a YuiQL query over the database. \
            \Filepaths are resolved to their absolute forms by default."
        )
    statisticsParser =
      info
        (pure Stats)
        (progDesc "Output statistics about the database.")
    auditParser =
      info
        (pure Audit)
        (progDesc "Audit the database. Read-only operation.")
    describeParser =
      info
        ( helper
            <*> subparser
              ( command "database" describeDatabaseParser
                  <> command "file" describeFilesParser
              )
        )
        ( progDesc
            "Describe the Descriptors in a database or \
            \show the tags on given files."
        )
     where
      describeDatabaseParser =
        info
          (pure (Describe DescribeDatabase))
          (progDesc "Show the structure of the database's Descriptor hierarchy.")
      describeFilesParser =
        info
          ( Describe . DescribeFiles
              <$> ( (UserSupplied <$> some (argument str (metavar "FILE_PATTERN")))
                      <|> pure GetFromStdIn
                  )
          )
          (progDesc "Show the tags attached to the files matching the given patterns.")
    addParser =
      info
        ( helper
            <*> subparser
              ( command "file" addFileParser
                  <> command "descriptor" addDescriptorParser
                  <> command "tag" addTagParser
              )
        )
        (progDesc "Add items to the database, use with -h for more info.")
     where
      addFileParser =
        info
          (Add <$> some (argument str (metavar "PATH")))
          (progDesc "Add all files found at the given paths to the database.")
      addDescriptorParser =
        info
          ( helper
              <*> ( Descriptors
                      <$> ( ( UserSupplied
                                <$> argument
                                  str
                                  ( metavar "YUIQL"
                                      <> help
                                        "An expression like \"a{b{c}}\" \
                                        \can define a new set of descriptors \
                                        \and their relations to one another. \
                                        \Reads from stdin if no expression is given."
                                  )
                            )
                              <|> pure GetFromStdIn
                          )
                  )
          )
          (progDesc "Add a YuiQL expression as descriptors to the database.")
      addTagParser =
        info
          ( helper
              <*> ( Tag
                      <$> argument
                        str
                        ( metavar "FILE_PATTERN"
                            <> help
                              "A pattern to match a file in the database with."
                        )
                        <*> argument str (metavar "YUIQL" <> help "A tag expression.")
                  )
          )
          ( progDesc
              "Tag a file or files matching the given pattern from the command line."
          )
    systemParser =
      info
        ( helper
            <*> subparser
              ( command "remove" systemRemoveParser
                  <> command "rename" systemRenameParser
                  <> command "DELETE" systemDeleteParser
              )
        )
        ( progDesc
            "Edit the files in a database, rename them in the filesystem, or delete them."
        )
     where
      systemRemoveParser =
        info
          (Remove <$> some (argument str (metavar "FILE_PATTERN")))
          (progDesc "Remove files matching the given patterns from just the database.")
      systemRenameParser =
        info
          ( helper
              <*> ( Move <$> argument str (metavar "FROM_PATH")
                      <*> argument str (metavar "TO_PATH")
                  )
          )
          ( progDesc
              "Rename a single file matching the given path \
              \to the new path in both the database and file system."
          )
      systemDeleteParser =
        info
          (helper <*> (Delete <$> some (argument str (metavar "FILE_PATTERN"))))
          ( progDesc
              "Delete files matching the given patterns \
              \from the database AND the file system. \
              \Always removes them from the database regardless of whether deletion \
              \was successful."
          )

data Program
  = Version
  | WithDB !FilePath !Command
  deriving (Show, Eq)

data Command
  = Default
  | Create
  | Add ![FilePath]
  | Descriptors !(StdInOptional String)
  | Move !FilePath !FilePath
  | Remove ![String]
  | Delete ![String]
  | Stats
  | Audit
  | Query !(StdInOptional String) !Bool
  | Tag !String !String
  | Describe !DescribeCommand
  deriving (Show, Eq)

data DescribeCommand
  = DescribeDatabase
  | DescribeFiles !(StdInOptional [String])
  deriving (Show, Eq)

data StdInOptional a = GetFromStdIn | UserSupplied a deriving (Show, Eq, Functor)

mainProgram :: Program -> IO ()
mainProgram Version = putStrLn . showVersion $ taggerVersion
mainProgram (WithDB dbPath Create) = do
  c <- openOrCreate dbPath
  close c
mainProgram (WithDB dbPath cm) = do
  absDbPath <- makeAbsolute dbPath
  withCurrentDirectory (takeDirectory absDbPath) $ do
    ec <- runExceptT $ open absDbPath
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
        Descriptors ss -> do
          ss' <- case ss of
            GetFromStdIn -> T.IO.getContents
            UserSupplied s -> pure . T.pack $ s
          r <- yuiQLCreateDescriptors c ss'
          either (T.IO.hPutStrLn stderr) pure r
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
        Query inpStr rel -> do
          q <- case inpStr of
            GetFromStdIn -> T.IO.getContents
            UserSupplied s -> pure $ T.pack s
          let (T.unpack -> connPath) = c ^. connName
          eQueryResults <- yuiQLFileQuery c q
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
        Main.Tag s (T.pack -> tExpr) -> do
          fs <- queryForFileByPattern (T.pack s) c
          mapM_ ((\fk -> yuiQLTagFile fk c tExpr) . fileId) fs
        Describe dc -> case dc of
          DescribeDatabase -> describeDatabaseDescriptors c
          DescribeFiles sio -> do
            s <- case sio of
              GetFromStdIn -> T.words <$> T.IO.getContents
              UserSupplied ss -> pure $ T.pack <$> ss
            fs <- concat <$> mapM (`queryForFileByPattern` c) s
            mapM_ (describeFile c) (fileId <$> fs)
        _alreadHandled -> pure ()

describeFile :: TaggedConnection -> RecordKey File -> IO ()
describeFile tc fk = do
  ctf <- runMaybeT $ queryForConcreteTaggedFileWithFileId fk tc
  case ctf of
    Just (ConcreteTaggedFile f hm) -> do
      T.IO.putStrLn . filePath $ f
      sequence_ $
        HRM.traverseHierarchyMap
          0
          (+ 1)
          ( \depth (ConcreteTag _ (Descriptor _ dp) _) children -> do
              T.IO.putStrLn $ T.replicate (2 * depth) " " <> dp <> " {"
              sequence_ children
              T.IO.putStrLn $ T.replicate (2 * depth) " " <> "}"
          )
          ( \depth ct ->
              T.IO.putStrLn $
                T.replicate (2 * depth) " "
                  <> (descriptor . concreteTagDescriptor $ ct)
          )
          (L.sortBy (compareConcreteTags hm))
          hm
      putStrLn ""
    Nothing -> pure ()

describeDatabaseDescriptors :: TaggedConnection -> IO ()
describeDatabaseDescriptors tc = do
  allD <- queryForDescriptorByPattern "#ALL#" tc
  mapM_ (describe' (0 :: Int)) allD
 where
  describe' depth (Descriptor dk dp) = do
    T.IO.putStr $ T.replicate (depth * 2) " " <> dp
    infra <- getInfraChildren dk tc
    if null infra
      then putStrLn ""
      else do
        T.IO.putStrLn $ " " <> "{"
        mapM_ (describe' (depth + 1)) infra
        T.IO.putStrLn $ T.replicate (depth * 2) " " <> "}"

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
