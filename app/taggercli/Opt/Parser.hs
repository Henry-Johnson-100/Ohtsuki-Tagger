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
import Control.Monad (filterM, join, unless, when, (<=<))
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
import System.IO (hPutStrLn, stderr)
import Text.TaggerQL
import Tagger.Shared

type CLICont r a = ContT r (ReaderT TaggedConnection IO) a

p' :: ParserInfo (IO ())
p' =
  info
    ( helper
        <*> ( runContWithDB <$> databasePathArgParser
                <*> ( ( auditParser
                          <|> (runQueryParser <|> addFileParser)
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

addFileParser :: Parser (CLICont r ())
addFileParser =
  switch (short 'a' <> long "add-files" <> help "Add files to the database.")
    *> (addFileCont <$> many (argument str idm))

addFileCont :: [FilePath] -> CLICont r ()
addFileCont fps =
  lift $ do
    tc <- ask
    liftIO . mapM_ (addFiles tc . T.pack) $ fps

removeFilesCont :: [FilePath] -> CLICont r ()
removeFilesCont fps =
  lift $ do
    tc <- ask
    relativeRealFPs <- prepareFilesForDatabase fps
    liftIO $ do
      fks <-
        map fileId
          . concat
          <$> mapM ((`queryForFileByPattern` tc) . T.pack) relativeRealFPs
      deleteFiles fks tc

{- |
 Given a list of file paths and a connection. Make the files relative to the name
 of the connection.
-}
prepareFilesForDatabase :: [FilePath] -> ReaderT TaggedConnection IO [FilePath]
prepareFilesForDatabase fps = do
  tc <- ask
  liftIO $ do
    dbConnAbs <- makeAbsolute . T.unpack $ tc ^. connName
    filteredRealFPs <- filterM filterNotRealFiles <=< mapM makeAbsolute $ fps
    return $ makeRelative dbConnAbs <$> filteredRealFPs
 where
  filterNotRealFiles fp' = do
    exists <- doesFileExist fp'
    if not exists
      then
        T.IO.hPutStrLn stderr (T.pack fp' <> " does not exist or is not visible.")
          >> return False
      else return True

databasePathArgParser :: Parser FilePath
databasePathArgParser =
  argument str (metavar "DATABASE")

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