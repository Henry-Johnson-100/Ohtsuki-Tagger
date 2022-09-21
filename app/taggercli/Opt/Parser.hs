{-# LANGUAGE StrictData #-}

module Opt.Parser (
  opts,
) where

import Options.Applicative
import Options.Applicative.Builder
import Options.Applicative.Types

data WithFilesCommand
  = AddFiles [FilePath]
  | RemoveFiles [FilePath]
  | DeleteFiles [FilePath]
  deriving (Show, Eq)

opts :: ParserInfo Bool
opts = info versionFlagParser idm

databasePathArgParser :: Parser FilePath
databasePathArgParser =
  argument str (metavar "DATABASE")

withFilesCommandParser :: Parser WithFilesCommand
withFilesCommandParser =
  subparser
    (command "add-files" (info addFilesParser idm))

-- (command "add" (info () _))

filePathListReader :: ReadM [FilePath]
filePathListReader = many str

-- do
-- s <- str :: ReadM String
-- undefined

addFilesParser :: Parser WithFilesCommand
addFilesParser =
  option
    (AddFiles <$> filePathListReader)
    ( short 'a'
        <> long "add-files"
        <> help "Add files to the database."
    )

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