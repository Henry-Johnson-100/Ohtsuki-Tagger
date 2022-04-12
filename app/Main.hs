{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Lens ((^.))
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
    queryWidget,
    themeConfig,
  )
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
                [ queryWidget,
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

main :: IO ()
main = do
  dbConn <- open "/home/monax/Repo/Haskell/tagger/images.db"
  execute_ dbConn "PRAGMA foreign_keys = on"
  runTaggerWindow dbConn
  close dbConn