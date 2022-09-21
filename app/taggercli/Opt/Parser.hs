{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Opt.Parser (
  TaggerCLIOptions (..),
  TaggerCLIReaderOptions (..),
  optsParser,
  p',
) where

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import qualified Data.Foldable as F
import Data.Functor
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Database.Tagger
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
    ( runContWithDB <$> databasePathArgParser
        <*> (runQueryParser <**> pure continueInDir)
    )
    idm

runContWithDB :: FilePath -> CLICont r r -> IO r
runContWithDB dp c = do
  absoluteDBPath <- makeAbsolute dp
  tc <- open absoluteDBPath
  (`runReaderT` tc) $ runContT c return

continueInDir :: CLICont r r -> CLICont r r
continueInDir c = do
  tc <- lift ask
  curDir <- liftIO getCurrentDirectory
  let (takeDirectory . T.unpack -> dbPath) = tc ^. connName
  liftIO $ setCurrentDirectory dbPath
  !contResult <- c
  liftIO $ setCurrentDirectory curDir
  return contResult

runQueryParser :: Parser (CLICont r ())
runQueryParser = runQuery <$> argument ((: []) <$> str) idm

runQuery :: [Text] -> CLICont r ()
runQuery (TaggerQLQuery . head -> q) = do
  tc <- lift ask
  let (T.unpack -> connPath) = tc ^. connName
  queryResults <- liftIO $ taggerQL q tc
  liftIO
    . mapM_
      ( (T.IO.putStrLn . T.pack <=< makeAbsolute)
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