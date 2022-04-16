{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <&>" #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close, open)
import Database.Tagger.Access (activateForeignKeyPragma)
import Event.Handler (taggerEventHandler)
import IO (getConfig, getEnv, hPutStrLn, stderr)
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
import Type.Config (TaggerConfig (dbPath))
import Type.Model
  ( HasDescriptorTree (descriptorTree),
    TaggedConnection (..),
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

runTaggerWindow :: TaggedConnection -> TaggerConfig -> IO ()
runTaggerWindow c cfg =
  startApp
    (emptyTaggerModel c cfg)
    taggerEventHandler
    taggerApplicationUI
    taggerApplicationConfig

getTaggedConnection :: FilePath -> IO TaggedConnection
getTaggedConnection p = do
  dbConn <- open p
  return (TaggedConnection (T.pack p) (Just dbConn))

closeTaggedConnection :: TaggedConnection -> IO ()
closeTaggedConnection (TaggedConnection _ mc) = maybe (pure ()) close mc

main :: IO ()
main = do
  userHome <- getEnv "HOME"
  let configPath = userHome ++ "/.config/tagger.toml"
  try' (getConfig configPath) $
    \config -> do
      dbConn <-
        getTaggedConnection
          . T.unpack
          . dbPath
          $ config
      -- activateForeignKeyPragma dbConn
      maybe (pure ()) activateForeignKeyPragma . connInstance $ dbConn
      runTaggerWindow dbConn config
      closeTaggedConnection dbConn
  where
    putEx = hPutStrLn stderr
    try' e c = runExceptT e >>= either putEx c
