{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
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
import Data.Either (lefts)
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
  deleteDescriptors,
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
  optional,
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
  yuiQLDeleteTags,
  yuiQLFileQuery,
  yuiQLTagFile,
 )
import Util (addFiles, compareConcreteTags)

data Program
  = Version
  | WithDB !FilePath !Command
  deriving (Show, Eq)

data Command
  = Default
  | Create
  | Add !AddCommand
  | Move !FilePath !FilePath
  | Remove !RemoveCommand
  | Delete ![String]
  | Stats
  | Audit
  | Query !(Maybe String) !Bool
  | Describe !DescribeCommand
  deriving (Show, Eq)

data DescribeCommand
  = DescribeDatabase
  | DescribeFiles !(Maybe [String])
  deriving (Show, Eq)

data AddCommand
  = AddCommandDescriptors !(Maybe [String])
  | AddCommandFiles !(Maybe [FilePath])
  | AddCommandTags !String !(Maybe [String])
  deriving (Show, Eq)

data RemoveCommand
  = RemoveCommandDescriptors !(Maybe [String])
  | RemoveCommandFiles !(Maybe [FilePath])
  | RemoveCommandTags !String !(Maybe [String])
  deriving (Show, Eq)

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
                  <> command "remove" removeParser
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
                    <$> optional
                      ( argument
                          str
                          ( metavar "YuiQL"
                              <> help
                                "A YuiQL query for files. \
                                \Reads from stdin if no query is given."
                          )
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
              <$> optional (some (argument str (metavar "FILE_PATTERN")))
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
          ( Add . AddCommandFiles
              <$> ( optional
                      . some
                      $ argument str (metavar "PATH")
                  )
          )
          (progDesc "Add all files found at the given paths to the database.")
      addDescriptorParser =
        info
          ( helper
              <*> ( Add . AddCommandDescriptors
                      <$> optional
                        ( some $
                            argument
                              str
                              ( metavar "YUIQL"
                                  <> help
                                    "An expression like \"a{b{c}}\" \
                                    \can define a new set of descriptors \
                                    \and their relations to one another. \
                                    \Reads from stdin if no expression is given."
                              )
                        )
                  )
          )
          (progDesc "Add a YuiQL expression as descriptors to the database.")
      addTagParser =
        info
          ( helper
              <*> ( Add
                      <$> ( AddCommandTags
                              <$> argument
                                str
                                ( metavar "YUIQL"
                                    <> help
                                      "A tag expression."
                                )
                                <*> ( optional . some $
                                        argument
                                          str
                                          ( metavar "FILE_PATTERN"
                                              <> help
                                                "Patterns to match files with. \
                                                \Reads from stdin if no \
                                                \expression is given."
                                          )
                                    )
                          )
                  )
          )
          ( progDesc
              "Tag a file or files matching the given pattern from the command line."
          )
    removeParser =
      info
        ( helper
            <*> subparser
              ( command "file" removeFileParser
                  <> command "descriptor" removeDescriptorParser
                  <> command "tag" removeTagParser
              )
        )
        (progDesc "Remove items from the database, use with -h for more info.")
     where
      removeFileParser =
        info
          ( helper
              <*> ( Remove . RemoveCommandFiles
                      <$> optional
                        ( some $
                            argument
                              str
                              ( metavar "FILE_PATTERN"
                                  <> help
                                    "Any files matching any of the given \
                                    \patterns will be removed from the database. \
                                    \Reads from stdin if there is no input."
                              )
                        )
                  )
          )
          (progDesc "Remove files matching the given patterns from the database.")
      removeDescriptorParser =
        info
          ( helper
              <*> ( Remove . RemoveCommandDescriptors
                      <$> optional
                        ( some $
                            argument
                              str
                              ( metavar "DESCRIPTOR_PATTERN"
                                  <> help
                                    "Any Descriptors matching any of the given \
                                    \patterns will be removed from the database. \
                                    \Reads from stdin if there is no input."
                              )
                        )
                  )
          )
          (progDesc "Remove Descriptors from the database.")
      removeTagParser =
        info
          ( helper
              <*> ( Remove
                      <$> ( RemoveCommandTags
                              <$> argument
                                str
                                ( metavar "YUIQL"
                                    <> help
                                      "A YuiQL Tag Expression to specify which \
                                      \tags to remove."
                                )
                              <*> optional
                                ( some $
                                    argument
                                      str
                                      ( metavar "FILE_PATTERN"
                                          <> help
                                            "Any files matching any of the given \
                                            \patterns are subject to tag removal. \
                                            \Reads from stdin if no input is given."
                                      )
                                )
                          )
                  )
          )
          ( progDesc
              "From the given file, \
              \remove tags matching the given YuiQL tag expression."
          )
    systemParser =
      info
        ( helper
            <*> subparser
              ( command "rename" systemRenameParser
                  <> command "DELETE" systemDeleteParser
              )
        )
        ( progDesc
            "Edit the files in a database, rename them in the filesystem, or delete them."
        )
     where
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

mainProgram :: Program -> IO ()
mainProgram Version = putStrLn . showVersion $ taggerVersion
mainProgram (WithDB dbPath Create) = do
  c <- openOrCreate dbPath
  close c
mainProgram (WithDB dbPath cm) = do
  absDbPath <- makeAbsolute dbPath
  withCurrentDirectory (takeDirectory absDbPath) $ do
    ec <- runExceptT $ open absDbPath
    either (T.IO.hPutStrLn stderr) (mainWithConnection cm) ec

mainWithConnection :: Command -> TaggedConnection -> IO ()
mainWithConnection comm c = case comm of
  Default -> runDefault
  Add ac -> runAddCommand ac
  Move s toN -> runMoveCommand s toN
  Remove rc -> runRemoveCommand rc
  Delete ss -> runDeleteCommand ss
  Stats -> runStatsCommand
  Audit -> runAuditCommand
  Query m_s b -> runQueryCommand m_s b
  Describe dc -> runDescribeCommand dc
  _requiresPathToHandle -> pure ()
 where
  runDefault = do
    defaultFile <- T.pack <$> getDataFileName focusedFileDefaultDataFile
    runTagger
      ( createTaggerModel
          c
          (Descriptor (-1) "fake descriptor")
          (Descriptor (-2) "fake #UNRELATED#")
          defaultFile
      )

  runAddCommand ac = case ac of
    AddCommandDescriptors sio -> do
      s <- case sio of
        Nothing -> fmap (: []) T.IO.getContents
        Just s -> pure . map T.pack $ s
      r <- mapM (yuiQLCreateDescriptors c) s
      mapM_ (T.IO.hPutStrLn stderr) . lefts $ r
    AddCommandFiles sio -> do
      s <- case sio of
        Nothing -> T.words <$> T.IO.getContents
        Just ss -> pure $ T.pack <$> ss
      mapM_ (addFiles c) s
    AddCommandTags (T.pack -> tagExpr) sio -> do
      fps <- case sio of
        Nothing -> T.words <$> T.IO.getContents
        Just ss -> pure $ T.pack <$> ss
      fs <- concat <$> mapM (`queryForFileByPattern` c) fps
      mapM_ (\f -> yuiQLTagFile (fileId f) c tagExpr) fs

  runMoveCommand s toN = do
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

  runRemoveCommand ss = case ss of
    RemoveCommandDescriptors m_ss -> do
      desPatterns <- maybe ((: []) <$> T.IO.getContents) (pure . map T.pack) m_ss
      ds <- concat <$> mapM (`queryForDescriptorByPattern` c) desPatterns
      deleteDescriptors (descriptorId <$> ds) c
    RemoveCommandFiles m_ss -> do
      removePaths <- maybe ((: []) <$> T.IO.getContents) (pure . map T.pack) m_ss
      fs <- concat <$> mapM (`queryForFileByPattern` c) removePaths
      deleteFiles (fileId <$> fs) c
    RemoveCommandTags s m_ss -> do
      filePathsToRemoveFrom <-
        maybe
          ((: []) <$> T.IO.getContents)
          (pure . map T.pack)
          m_ss
      fs <- concat <$> mapM (`queryForFileByPattern` c) filePathsToRemoveFrom
      r <- yuiQLDeleteTags c (fileId <$> fs) (T.pack s)
      either (T.IO.hPutStrLn stderr) pure r

  runDeleteCommand ss = do
    fs <- concat <$> mapM ((`queryForFileByPattern` c) . T.pack) ss
    mapM_ (rmFile c . fileId) fs

  runStatsCommand = runReaderT showStats c

  runAuditCommand = runReaderT mainReportAudit c

  runQueryCommand inpStr rel = do
    q <- case inpStr of
      Nothing -> T.IO.getContents
      Just s -> pure $ T.pack s
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

  runDescribeCommand dc = case dc of
    DescribeDatabase -> describeDatabaseDescriptors c
    DescribeFiles sio -> do
      s <- case sio of
        Nothing -> T.words <$> T.IO.getContents
        Just ss -> pure $ T.pack <$> ss
      fs <- concat <$> mapM (`queryForFileByPattern` c) s
      mapM_ (describeFile c) (fileId <$> fs)

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
