{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant <$>" #-}

module CLI.Parser (
  taggerExParser,
) where

import Data.Functor (($>))
import Data.Monoid (Any (..))
import CLI.Data (
  TaggerCommand (TaggerCommand),
  TaggerDBCommand (TaggerDBCommand),
  TaggerEx (..),
  TaggerQueryCommand (TaggerQueryCommand),
 )
import Options.Applicative (
  Alternative ((<|>)),
  Parser,
  ParserInfo,
  argument,
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
  switch,
 )

taggerExParser :: ParserInfo TaggerEx
taggerExParser =
  info
    ( helper
        <*> showVersionParser
        <|> (TaggerExDB <$> databasePathArgParser <*> taggerDBCommandParser)
    )
    ( header
        "TaggerCLI"
        <> progDesc
          "Allows a user to perform a limited set of \
          \actions on a tagger database.\n\
          \It is also possible to print some stats and run an audit."
    )

taggerDBCommandParser :: Parser TaggerDBCommand
taggerDBCommandParser =
  TaggerDBCommand
    <$> auditParser
      <*> statsParser
      <*> taggerCommandParser

taggerCommandParser :: Parser TaggerCommand
taggerCommandParser =
  TaggerCommand
    <$> optional taggerQueryCommandParser
 where
  taggerQueryCommandParser =
    TaggerQueryCommand
      <$> argument str (metavar "QUERY")
        <*> ( Any
                <$> switch
                  ( long "relative"
                      <> help
                        "Output filepaths as they are\
                        \ stored in the database."
                  )
            )

auditParser :: Parser Any
auditParser =
  Any
    <$> switch (long "audit" <> help "Audit the database.")

showVersionParser :: Parser TaggerEx
showVersionParser =
  switch (short 'v' <> long "version" <> help "Show the version")
    $> TaggerExVersion

statsParser :: Parser Any
statsParser =
  Any
    <$> switch (long "stats" <> help "Show statistics about the database")

-- addFileParser :: Parser (ReaderT TaggedConnection (ContT () IO) ())
-- addFileParser =
--   switch
--     ( short 'a' <> long "add-files"
--         <> help
--           "Add files to the database. \
--           \Recursively add any files found at PATH."
--     )
--     *> (addFileCont <$> many (argument str (metavar "PATH")))

-- addFileCont :: [FilePath] -> ReaderT TaggedConnection (ContT () IO) ()
-- addFileCont fps =
--   do
--     tc <- ask
--     liftIO . mapM_ (addFiles tc . T.pack) $ fps

databasePathArgParser :: Parser FilePath
databasePathArgParser =
  argument str (metavar "DATABASE")

-- showAppliedTagsParser =
--   switch (long "show-tags" <> help "Show tags applied to the given files")
--     *> (showAppliedTagsCont <$> many (argument str (metavar "PATH")))

-- showAppliedTagsCont :: [FilePath] -> ReaderT TaggedConnection (ContT () IO) ()
-- showAppliedTagsCont fps = do
--   tc <- ask
--   fs <- liftIO . fmap concat . mapM (flip queryForFileByPattern tc . T.pack) $ fps
--   cfs <- mapReaderT liftIO $ getConcreteFiles fs
--   liftIO $ mapM_ reportTags cfs