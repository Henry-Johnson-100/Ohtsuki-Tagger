{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <&>" #-}

module Main where

import Control.Applicative (Alternative (empty))
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close, execute_, open)
import Database.Tagger.Access (Connection)
import Event.Handler (taggerEventHandler)
import Monomer
  ( AppConfig,
    WidgetEnv,
    WidgetNode,
    appInitEvent,
    button,
    hgrid,
    startApp,
    vgrid,
    vstack,
  )
import Node.Application
  ( configPanel,
    descriptorTreeQuadrantWidget,
    fileSelectionWidget,
    fileSinglePreviewWidget,
    queryAndTagEntryWidget,
    themeConfig,
  )
import System.Directory
import System.IO
import Toml
import Type.Config
import Type.Model
  ( HasDescriptorTree (descriptorTree),
    TaggerEvent (DebugPrintSelection, TaggerInit),
    TaggerModel,
    emptyTaggerModel,
    fileSelection,
    unrelatedDescriptorTree,
  )

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  WidgetNode TaggerModel TaggerEvent
taggerApplicationUI wenv model = widgetTree
  where
    widgetTree =
      hgrid
        [ vgrid
            [ fileSelectionWidget (model ^. fileSelection),
              vstack
                [ queryAndTagEntryWidget,
                  descriptorTreeQuadrantWidget
                    (model ^. descriptorTree)
                    (model ^. unrelatedDescriptorTree)
                ]
            ],
          vgrid
            [ vstack [configPanel, button "print selection" DebugPrintSelection],
              fileSinglePreviewWidget model
            ]
        ]

taggerApplicationConfig :: [AppConfig TaggerEvent]
taggerApplicationConfig =
  appInitEvent TaggerInit : themeConfig

runTaggerWindow :: Connection -> IO ()
runTaggerWindow c =
  startApp
    (emptyTaggerModel c)
    taggerEventHandler
    taggerApplicationUI
    taggerApplicationConfig

type ConfigException = String

maybeAppend :: Maybe T.Text -> T.Text -> T.Text
maybeAppend x y = maybe y (`T.append` y) x

validatePath :: FilePath -> ExceptT ConfigException IO FilePath
validatePath p = do
  isFile <- lift . doesFileExist $ p
  if isFile then lift . makeAbsolute $ p else throwE $ "File not found: " ++ p

fileNotFoundError = (++) "File not found: "

dirNotFoundError = (++) "Directory not found: "

validateDir :: FilePath -> ExceptT ConfigException IO FilePath
validateDir p = do
  isDir <- lift . doesDirectoryExist $ p
  if isDir then lift . makeAbsolute $ p else throwE $ "Directory not found: " ++ p

getConfig :: String -> ExceptT String IO TaggerConfig
getConfig =
  withExceptT
    (T.unpack . prettyTomlDecodeErrors)
    . except
    <=< decodeFileEither taggerConfigCodec
    <=< validatePath

getConfigDbConn :: FilePath -> ExceptT ConfigException IO FilePath
getConfigDbConn =
  withExceptT
    ("Configuration error when opening database connection:\n" ++)
    . validatePath

validateConfig :: TaggerConfig -> ExceptT ConfigException IO TaggerConfig
validateConfig (TaggerConfig dbc sc) = do
  validatedDatabaseConfig <- validateDatabaseConfig dbc
  validatedStyleConfig <- validateStyleConfig sc
  withExceptT ("Error validating the configuration file:\n" ++)
    . return
    $ TaggerConfig validatedDatabaseConfig validatedStyleConfig
  where
    validateDatabaseConfig (TaggerDatabaseConfig dbpc) =
      validateDatabasePathConfig dbpc >>= return . TaggerDatabaseConfig
    validateDatabasePathConfig (TaggerDatabasePathConfig d db b s) = do
      dbDir <- maybe (return "") (validateDir . T.unpack) d
      let inDir = fmap T.pack . validatePath . (++) dbDir . T.unpack
      dbp <- inDir db
      bp <- inDir b
      sp <- inDir s
      withExceptT ("Error validating database paths:\n" ++) . return $
        TaggerDatabasePathConfig (Just . T.pack $ dbDir) dbp bp sp
    validateStyleConfig (TaggerStyleConfig sfc) =
      validateStyleFontConfig sfc >>= return . TaggerStyleConfig
    validateStyleFontConfig (TaggerStyleFontConfig d t r b) = do
      styleFontDir <- maybe (return "") (validateDir . T.unpack) d
      let inDir = fmap T.pack . validatePath . (++) styleFontDir . T.unpack
      thinPath <- inDir t
      regPath <- inDir r
      boldPath <- inDir b
      withExceptT ("Error validating font paths:\n" ++) . return $
        TaggerStyleFontConfig (Just . T.pack $ styleFontDir) thinPath regPath boldPath

main :: IO ()
main = do
  try' (getConfig "/home/monax/.config/tagger_config.toml" >>= validateConfig) $
    \config -> do
      dbConn <-
        open
          . T.unpack
          . databasePathDatabase
          . databasePathConfig
          . databaseConfig
          $ config
      execute_ dbConn "PRAGMA foreign_keys = on"
      runTaggerWindow dbConn
      close dbConn
  where
    putEx = hPutStrLn stderr
    try' e c = runExceptT e >>= either putEx c
