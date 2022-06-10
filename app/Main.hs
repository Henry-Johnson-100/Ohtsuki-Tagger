{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Lens ((%~), (&), (^.))
import Control.Monad
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Foldable as F
import qualified Data.Text as T
import Database.SQLite.Simple (close, open)
import Event.CLI
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
taggerApplicationUI _ model =
  globalKeystrokes
    . flip styleBasic [padding 0]
    . vstack
    $ [ menubar,
        zstack
          [ visibility model Config (configConfigurationPage model),
            visibility model Database databaseConfigurePage,
            visibility model Selection selectionConfigurePage,
            visibility model ProgramVisibilityDescriptor . descriptorConfigurePage $
              model,
            visibility model Main
              . vsplit_ [splitIgnoreChildResize True]
              $ ( fileSingleWidget model,
                  box_ [alignBottom] $
                    hgrid
                      [ vstack
                          [ descriptorTreeQuadrantWidget
                              ( model
                                  ^. ( programConfig
                                         . descriptorTreeConf
                                     )
                              )
                              ( model
                                  ^. ( descriptorModel
                                         . mainDescriptorTree
                                         . rootTree
                                     )
                              )
                              ( model
                                  ^. ( descriptorModel
                                         . unrelatedDescriptorTree
                                         . rootTree
                                     )
                              )
                          ],
                        operationWidget,
                        fileSelectionWidget model
                      ]
                )
          ]
      ]

taggerApplicationConfig :: TaggerConfig -> [AppConfig TaggerEvent]
taggerApplicationConfig cfg =
  appInitEvent TaggerInit : themeConfig (cfg ^. styleConf)

runTaggerWindow :: TaggerConfig -> IO ()
runTaggerWindow cfg =
  startApp
    (emptyTaggerModel cfg)
    taggerEventHandler
    taggerApplicationUI
    (taggerApplicationConfig cfg)

main :: IO ()
main = do
  rawArgs <- getArgs
  opts <- getOptionRecord rawArgs
  configPath <- getConfigPath
  let configExcept = getConfig configPath
  try' configExcept $ \config' -> do
    let config =
          config' & dbconf . dbconfPath
            %~ (maybe id (const . T.pack) . optionDatabasePath $ opts)
    conn <- open . T.unpack $ config ^. dbconf . dbconfPath
    F.sequenceA_ $ [printHelp, printVersion, cliQuery conn, cliOperateOnFile conn] <*> [opts]
    close conn
    when (optionRunTagger opts) $ runTaggerWindow config
  where
    try' e c = runExceptT e >>= either (hPrint stderr) c
