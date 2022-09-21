{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Opt.Parser (
  TaggerCLIOptions (..),
  TaggerCLIReaderOptions (..),
  optsParser,
  p',
) where

import Control.Lens ((^.))
import Control.Monad (join, unless, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import qualified Data.Foldable as F
import Data.Functor
import qualified Data.HashSet as HS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger
import Opt
import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Types
import System.Directory
import System.FilePath
import Text.TaggerQL

type CLICont r a = ContT r (ReaderT TaggedConnection IO) a

p' :: ParserInfo (IO ())
p' =
  info
    ( helper
        <*> ( runContWithDB <$> databasePathArgParser
                <*> ( ( auditParser
                          <|> runQueryParser
                      )
                        <**> pure continueInDir
                    )
            )
    )
    idm

runContWithDB :: FilePath -> CLICont () () -> IO ()
runContWithDB dp c = void . join . evalContT . callCC $ \exit -> do
  absoluteDBPath <- liftIO $ makeAbsolute dp
  absPathExists <- liftIO $ doesFileExist absoluteDBPath
  unless absPathExists $ exit (T.IO.putStrLn $ "No file exists at: " <> T.pack dp)
  tc <- liftIO $ open' absoluteDBPath
  return $ (`runReaderT` tc) (runContT c return)

continueInDir :: CLICont r r -> CLICont r r
continueInDir c = do
  tc <- lift ask
  curDir <- liftIO getCurrentDirectory
  let (takeDirectory . T.unpack -> dbPath) = tc ^. connName
  liftIO $ setCurrentDirectory dbPath
  !contResult <- c
  liftIO $ setCurrentDirectory curDir
  return contResult

auditParser :: Parser (CLICont r ())
auditParser = switch (long "audit" <> help "Audit the database.") $> lift mainReportAudit

runQueryParser :: Parser (CLICont r ())
runQueryParser =
  flip runQuery
    <$> option
      ((: []) <$> str)
      ( short 'q'
          <> long "query"
          <> help "Run a query on the database using TaggerQL"
      )
      <*> switch (long "absolute" <> help "Report query results with absolute paths.")

runQuery :: Bool -> [Text] -> CLICont r ()
runQuery makeAbs (TaggerQLQuery . head -> q) = do
  tc <- lift ask
  let (T.unpack -> connPath) = tc ^. connName
  queryResults <- liftIO $ taggerQL q tc
  if HS.null queryResults
    then liftIO . T.IO.putStrLn $ "No results."
    else
      liftIO
        . mapM_
          ( (T.IO.putStrLn . T.pack <=< if makeAbs then makeAbsolute else pure)
              . makeRelative connPath
              . T.unpack
              . filePath
          )
        . sortOn filePath
        . F.toList
        $ queryResults

optsParser :: ParserInfo TaggerCLIOptions
optsParser =
  info
    ( helper
        <*> ( versionFlagParser $> TCLIVersion
                <|> TCLIOnDatabase <$> taggerCLIReaderOptionsParser
            )
    )
    (header "TAGGERCLI")

data TaggerCLIOptions
  = TCLIVersion
  | TCLIOnDatabase TaggerCLIReaderOptions
  deriving (Show, Eq)

{- |
 records specified in the sequence that serial options should occur
-}
data TaggerCLIReaderOptions = TaggerCLIReaderOptions
  { tCLIDBPath :: FilePath
  , tCLIFileCommand :: [WithFilesCommand]
  , tCLIDescriptorCommand :: [WithDescriptorsCommand]
  , tCLIQueryLikeCommand :: [QueryLikeCommand]
  }
  deriving (Show, Eq)

data QueryLikeCommand = Query Text deriving (Show, Eq)

data WithDescriptorsCommand = AddDescriptors [Text] deriving (Show, Eq)

data WithFilesCommand
  = AddFiles [FilePath]
  | RemoveFiles [FilePath]
  | DeleteFiles [FilePath]
  deriving (Show, Eq)

databasePathArgParser :: Parser FilePath
databasePathArgParser =
  argument str (metavar "DATABASE")

taggerCLIReaderOptionsParser :: Parser TaggerCLIReaderOptions
taggerCLIReaderOptionsParser =
  TaggerCLIReaderOptions
    <$> databasePathArgParser
    <*> many withFilesCommandParser
    <*> pure mempty
    <*> pure mempty

withFilesCommandParser :: Parser WithFilesCommand
withFilesCommandParser =
  addFilesParser
    <|> removeFilesParser
    <|> deleteFilesParser

addFilesParser :: Parser WithFilesCommand
addFilesParser =
  AddFiles
    <$> option
      filePathListReader
      ( short 'a'
          <> long "add-files"
          <> help "Add the list of given files to the database."
      )

removeFilesParser :: Parser WithFilesCommand
removeFilesParser =
  RemoveFiles
    <$> option
      filePathListReader
      ( short 'r'
          <> long "remove-files"
          <> help "Remove the given list of files from the database"
      )

deleteFilesParser :: Parser WithFilesCommand
deleteFilesParser =
  DeleteFiles
    <$> option
      filePathListReader
      ( long "delete-files"
          <> help "Delete the given list of files from the database AND the filesystem"
      )

filePathListReader :: ReadM [FilePath]
filePathListReader = (: []) <$> str

versionFlagParser :: Parser Bool
versionFlagParser =
  switch
    ( long "version"
        <> short 'v'
        <> help "Print the version number."
    )

auditFlagParser :: Parser Bool
auditFlagParser =
  switch
    ( long "audit"
        <> help
          "Audit the database, performing a series of queries and \
          \printing the results."
    )