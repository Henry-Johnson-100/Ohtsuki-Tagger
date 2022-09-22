{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Opt.Parser (
  p',
) where

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Reader (
  ReaderT (runReaderT),
  ask,
  mapReaderT,
 )
import qualified Data.Foldable as F
import Data.Functor (($>))
import qualified Data.HashSet as HS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Version (showVersion)
import Database.Tagger (
  File (filePath),
  HasConnName (connName),
  TaggedConnection,
  open,
  open',
 )
import Opt (mainReportAudit, showStats)
import Options.Applicative (
  Alternative (many, (<|>)),
  Parser,
  ParserInfo,
  argument,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  str,
  switch,
 )
import System.Directory (
  doesFileExist,
  getCurrentDirectory,
  makeAbsolute,
  setCurrentDirectory,
 )
import System.FilePath (makeRelative, takeDirectory)
import System.IO (stderr)
import Tagger.Info (taggerVersion)
import Tagger.Shared (addFiles)
import Text.TaggerQL (TaggerQLQuery (TaggerQLQuery), taggerQL)

p' :: ParserInfo (ContT () IO ())
p' =
  info
    ( helper
        <*> ( showVersionParser
                <|> ( runFutureReader
                        <$> databasePathArgParser
                          <*> createDBSwitch
                          <*> ( continueInDir
                                  <$> ( ( auditParser
                                            <|> showStatsParser
                                        )
                                          <|> runQueryParser
                                          <|> addFileParser
                                      )
                              )
                    )
            )
    )
    ( header
        "TaggerCLI"
        <> progDesc
          "Allows a user to perform a limited set of \
          \actions on a tagger database.\n\
          \It is also possible to print some stats and run an audit."
    )

runFutureReader ::
  FilePath ->
  Bool ->
  ReaderT TaggedConnection (ContT r IO) () ->
  ContT r IO ()
runFutureReader fp createIfNotExists r = do
  absPath <- liftIO $ makeAbsolute fp
  absPathExists <- liftIO $ doesFileExist absPath
  if not absPathExists && not createIfNotExists
    then liftIO . T.IO.hPutStrLn stderr $ "No such file exists: " <> T.pack fp
    else do
      tc <- liftIO $ (if createIfNotExists then open else open') absPath
      runReaderT r tc

createDBSwitch :: Parser Bool
createDBSwitch =
  switch
    ( long "create"
        <> help
          "Create a database at the given location if none exists. \
          \Otherwise, will run patches, initialization, etc. if required."
    )

continueInDir ::
  ReaderT TaggedConnection (ContT () IO) () ->
  ReaderT TaggedConnection (ContT () IO) ()
continueInDir c = do
  tc <- ask
  curDir <- liftIO getCurrentDirectory
  let (takeDirectory . T.unpack -> dbPath) = tc ^. connName
  liftIO $ setCurrentDirectory dbPath
  !contResult <- c
  liftIO $ setCurrentDirectory curDir
  return contResult

auditParser :: Parser (ReaderT TaggedConnection (ContT () IO) ())
auditParser = switch (long "audit" <> help "Audit the database.") $> auditCont

auditCont :: ReaderT TaggedConnection (ContT () IO) ()
auditCont = mapReaderT liftIO mainReportAudit

showVersionParser :: Parser (ContT () IO ())
showVersionParser =
  switch (short 'v' <> long "version" <> help "Show the version")
    $> showVersionCont

showVersionCont :: ContT () IO ()
showVersionCont =
  liftIO . T.IO.putStrLn . T.pack . showVersion $ taggerVersion

showStatsParser :: Parser (ReaderT TaggedConnection (ContT () IO) ())
showStatsParser =
  switch (long "stats" <> help "Show statistics about the database")
    $> showStatsCont

showStatsCont :: ReaderT TaggedConnection (ContT () IO) ()
showStatsCont = mapReaderT liftIO showStats

runQueryParser :: Parser (ReaderT TaggedConnection (ContT () IO) ())
runQueryParser =
  flip runQuery
    <$> ( argument ((: []) <$> str) (metavar "QUERY")
            <|> option
              ((: []) <$> str)
              ( short 'q'
                  <> long "query"
                  <> help "Run a query on the database using TaggerQL"
                  <> metavar "QUERY"
              )
        )
      <*> switch (long "absolute" <> help "Report query results with absolute paths.")

runQuery :: Bool -> [Text] -> ReaderT TaggedConnection (ContT () IO) ()
runQuery makeAbs (TaggerQLQuery . head -> q) =
  do
    tc <- ask
    let (T.unpack -> connPath) = tc ^. connName
    liftIO $ do
      queryResults <- taggerQL q tc
      if HS.null queryResults
        then T.IO.hPutStrLn stderr "No results."
        else
          mapM_
            ( ( T.IO.putStrLn . T.pack
                  <=< if makeAbs
                    then makeAbsolute
                    else pure
              )
                . makeRelative connPath
                . T.unpack
                . filePath
            )
            . sortOn filePath
            . F.toList
            $ queryResults

addFileParser :: Parser (ReaderT TaggedConnection (ContT () IO) ())
addFileParser =
  switch
    ( short 'a' <> long "add-files"
        <> help
          "Add files to the database. \
          \Recursively add any files found at PATH."
    )
    *> (addFileCont <$> many (argument str (metavar "PATH")))

addFileCont :: [FilePath] -> ReaderT TaggedConnection (ContT () IO) ()
addFileCont fps =
  do
    tc <- ask
    liftIO . mapM_ (addFiles tc . T.pack) $ fps

databasePathArgParser :: Parser FilePath
databasePathArgParser =
  argument str (metavar "DATABASE")
