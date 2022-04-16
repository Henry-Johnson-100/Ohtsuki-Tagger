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
import Database.SQLite.Simple (Connection (connectionHandle), close, execute_, open)
import qualified Database.SQLite3 as DirectSqlite
import Database.Tagger.Access (Connection, activateForeignKeyPragma)
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
import System.Environment
import System.IO
import System.Process
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

initializeDatabase :: FilePath -> FilePath -> p -> IO ()
initializeDatabase scriptLoc dbLoc backupLoc = do
  memConn <- open ":memory:"
  let memConnHandle = connectionHandle memConn
  scriptHandle <- openFile scriptLoc ReadMode
  scriptContents <- hGetContents scriptHandle
  newDbConn <- do
    touch dbLoc
    open dbLoc
  DirectSqlite.exec memConnHandle . T.pack $ scriptContents
  hClose scriptHandle
  newDbSave <-
    DirectSqlite.backupInit
      (connectionHandle newDbConn)
      "main"
      memConnHandle
      "main"
  DirectSqlite.backupStep newDbSave (-1)
  DirectSqlite.backupFinish newDbSave
  close memConn
  close newDbConn

touch :: FilePath -> IO ()
touch = hClose <=< flip openFile WriteMode

backupDbConn :: Connection -> FilePath -> IO ()
backupDbConn c backupTo = do
  let currentHandle = connectionHandle c
  doesFileExist backupTo >>= flip unless (touch backupTo)
  backupHandle <- fmap connectionHandle . open $ backupTo
  backupProcess <- DirectSqlite.backupInit backupHandle "main" currentHandle "main"
  DirectSqlite.backupStep backupProcess (-1)
  DirectSqlite.backupFinish backupProcess

main :: IO ()
main = do
  userHome <- getEnv "HOME"
  try' (getConfig (userHome ++ "/.config/tagger.toml")) $
    \config -> do
      dbConn <-
        open
          . T.unpack
          . dbPath
          $ config
      activateForeignKeyPragma dbConn
      runTaggerWindow dbConn
      close dbConn
  where
    putEx = hPutStrLn stderr
    try' e c = runExceptT e >>= either putEx c
