{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close, open)
import Database.Tagger.Access (activateForeignKeyPragma)
import Event.Handler (taggerEventHandler)
import IO
import Monomer
import Node.Application
import Type.Config
import Type.Model

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  WidgetNode TaggerModel TaggerEvent
taggerApplicationUI wenv model' = widgetTree
  where
    widgetTree =
      let !model = model'
       in vstack
            [ menubar,
              zstack
                [ visibility model Configure configureZone,
                  visibility model Main
                    . vgrid
                    $ [ box_ [alignMiddle] . fileSinglePreviewWidget $ model,
                        hgrid
                          [ vstack
                              [ descriptorTreeQuadrantWidget
                                  (model ^. descriptorTree)
                                  (model ^. unrelatedDescriptorTree)
                              ],
                            operationWidget,
                            fileSelectionWidget (model ^. fileSelection)
                          ]
                      ]
                ]
            ]
            `styleBasic` [padding 0]

taggerApplicationConfig :: [AppConfig TaggerEvent]
taggerApplicationConfig =
  appInitEvent TaggerInit : themeConfig

runTaggerWindow :: TaggerConfig -> IO ()
runTaggerWindow cfg =
  startApp
    (emptyTaggerModel cfg)
    taggerEventHandler
    taggerApplicationUI
    taggerApplicationConfig

main :: IO ()
main = do
  configPath <- getConfigPath
  try' (getConfig configPath) runTaggerWindow
  where
    try' e c = runExceptT e >>= either (hPutStrLn stderr) c
