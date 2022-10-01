{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant <$>" #-}

module Opt.Parser (
  taggerExParser,
) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import qualified Data.Foldable as F
import Data.Functor (($>))
import qualified Data.HashSet as HS
import Data.List
import Data.Monoid (Any (..))
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Version
import Database.Tagger

-- import Opt

import Data.Maybe
import Opt.Data
import Opt.Data.Lens
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import Tagger.Info
import Tagger.Shared
import Text.TaggerQL

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
    <$> strListParser
      (Just 'a')
      "add-files"
      "Add files to the database"
      Nothing
    <*> strListParser
      Nothing
      "remove-files"
      "Remove files from the database"
      Nothing
    <*> strListParser
      Nothing
      "delete-files"
      "Delete files from the database and filesystem"
      Nothing
    <*> strListTupleParser
      (Just 'm')
      "move-files"
      "Rename and move files in the filesystem"
      Nothing
      Nothing
    <*> strListParser
      (Just 's')
      "show-tags"
      "Show the tags of the given files"
      Nothing
    <*> strListParser
      (Just 'd')
      "add-descriptors"
      "Add new descriptors to the database"
      (Just "DESCRIPTORS")
    <*> strListParser
      Nothing
      "delete-descriptors"
      "Delete descriptors from the database"
      (Just "DESCRIPTORS")
    <*> strListTupleParser
      (Just 't')
      "tag"
      "Apply the given TaggerQL statement to the given files"
      (Just "TAGGERQL")
      (Just "FILES")
    <*> strListTupleParser
      (Just 'r')
      "relate"
      "Relate descriptors, first given is meta to the second"
      (Just "DESCRIPTOR")
      (Just "DESCRIPTOR")
 where
  strListParser s l h m =
    ( switch (maybe idm short s <> long l <> help h)
        *> some (argument str (metavar $ fromMaybe "FILES" m))
    )
      <|> pure mempty
  strListTupleParser s l h m1 m2 =
    ( switch (maybe idm short s <> long l <> help h)
        *> some
          ( (,) <$> argument str (metavar $ fromMaybe "FILE" m1)
              <*> argument str (metavar $ fromMaybe "FILE" m2)
          )
    )
      <|> pure mempty

--   subparser
--     ( command
--         "report"
--         ( info
--             ( (\fs -> mempty & reportTags .~ fs)
--                 <$> ( switch (long "show-tags" <> help "Show the tags of the given files")
--                         *> some (argument str (metavar "FILES"))
--                     )
--             )
--             idm
--         )
--     )
--  where

-- p' :: ParserInfo (ContT () IO ())
-- p' =
--   info
--     ( helper
--         <*> ( showVersionParser
--                 <|> ( runFutureReader
--                         <$> databasePathArgParser
--                           <*> createDBSwitch
--                           <*> ( continueInDir
--                                   <$> ( ( auditParser
--                                             <|> showStatsParser
--                                         )
--                                           <|> runQueryParser
--                                           <|> addFileParser
--                                           <|> showAppliedTagsParser
--                                       )
--                               )
--                     )
--             )
--     )
--     ( header
--         "TaggerCLI"
--         <> progDesc
--           "Allows a user to perform a limited set of \
--           \actions on a tagger database.\n\
--           \It is also possible to print some stats and run an audit."
--     )

-- runFutureReader ::
--   FilePath ->
--   Bool ->
--   ReaderT TaggedConnection (ContT r IO) () ->
--   ContT r IO ()
-- runFutureReader fp createIfNotExists r = do
--   absPath <- liftIO $ makeAbsolute fp
--   absPathExists <- liftIO $ doesFileExist absPath
--   if not absPathExists && not createIfNotExists
--     then liftIO . T.IO.hPutStrLn stderr $ "No such file exists: " <> T.pack fp
--     else do
--       tc <- liftIO $ (if createIfNotExists then open else open') absPath
--       runReaderT r tc

-- createDBSwitch :: Parser Bool
-- createDBSwitch =
--   switch
--     ( long "create"
--         <> help
--           "Create a database at the given location if none exists. \
--           \Otherwise, will run patches, initialization, etc. if required."
--     )

-- continueInDir ::
--   ReaderT TaggedConnection (ContT () IO) () ->
--   ReaderT TaggedConnection (ContT () IO) ()
-- continueInDir c = do
--   tc <- ask
--   curDir <- liftIO getCurrentDirectory
--   let (takeDirectory . T.unpack -> dbPath) = tc ^. connName
--   liftIO $ setCurrentDirectory dbPath
--   !contResult <- c
--   liftIO $ setCurrentDirectory curDir
--   return contResult

auditParser :: Parser Any
auditParser =
  Any
    <$> switch (long "audit" <> help "Audit the database.")

-- auditCont :: ReaderT TaggedConnection (ContT () IO) ()
-- auditCont = mapReaderT liftIO mainReportAudit

showVersionParser :: Parser TaggerEx
showVersionParser =
  switch (short 'v' <> long "version" <> help "Show the version")
    $> TaggerExVersion

-- showVersionCont :: ContT () IO ()
-- showVersionCont =
--   liftIO . T.IO.putStrLn . T.pack . showVersion $ taggerVersion

-- showStatsParser :: Parser (ReaderT TaggedConnection (ContT () IO) ())
-- showStatsParser =
--   switch (long "stats" <> help "Show statistics about the database")
--     $> showStatsCont

statsParser :: Parser Any
statsParser =
  Any
    <$> switch (long "stats" <> help "Show statistics about the database")

-- showStatsCont :: ReaderT TaggedConnection (ContT () IO) ()
-- showStatsCont = mapReaderT liftIO showStats

-- runQueryParser :: Parser (ReaderT TaggedConnection (ContT () IO) ())
-- runQueryParser =
--   flip runQuery
--     <$> argument str (metavar "QUERY")
--       <*> switch
--         ( long "relative"
--             <> help
--               "Report query results with relative paths. \
--               \The files are stored with their paths relative to the database file \
--               \then made absolute by default when reporting query results."
--         )

-- runQuery :: Bool -> Text -> ReaderT TaggedConnection (ContT () IO) ()
-- runQuery leaveRelative (TaggerQLQuery -> q) =
--   do
--     tc <- ask
--     let (T.unpack -> connPath) = tc ^. connName
--     liftIO $ do
--       queryResults <- taggerQL q tc
--       if HS.null queryResults
--         then T.IO.hPutStrLn stderr "No results."
--         else
--           mapM_
--             ( ( T.IO.putStrLn . T.pack
--                   <=< if leaveRelative
--                     then pure
--                     else makeAbsolute
--               )
--                 . makeRelative connPath
--                 . T.unpack
--                 . filePath
--             )
--             . sortOn filePath
--             . F.toList
--             $ queryResults

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