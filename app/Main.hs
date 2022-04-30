{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Lens ((^.))
import Control.Monad
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
taggerApplicationUI wenv model' =
  let !model = model'
   in vstack
        [ menubar,
          zstack
            [ visibility model Config configConfigurationPage,
              visibility model Database databaseConfigurePage,
              visibility model Selection selectionConfigurePage,
              visibility model ProgramVisibilityDescriptor . descriptorConfigurePage $
                model,
              visibility model Main
                . vsplit_ [splitIgnoreChildResize True]
                $ ( fileSingleWidget
                      (model ^. doSoloTag)
                      (model ^. (fileSelectionModel . fileSelection))
                      (model ^. singleFileModel),
                    box_ [alignBottom] . hsplit_ [splitIgnoreChildResize True] $
                      ( hgrid
                          [ vstack
                              [ descriptorTreeQuadrantWidget
                                  (model ^. (programConfig . descriptorTreeConf))
                                  (model ^. (descriptorModel . mainDescriptorTree . rootTree))
                                  (model ^. (descriptorModel . unrelatedDescriptorTree . rootTree))
                              ],
                            operationWidget
                          ],
                        fileSelectionWidget model
                      )
                  )
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
  rawArgs <- getArgs
  let hasVersionFlag = or $ flip elem rawArgs <$> ["-v", "--version"]
  when hasVersionFlag (putStrLn taggerVersion)
  unless hasVersionFlag $ do
    configPath <- getConfigPath
    try' (getConfig configPath) runTaggerWindow
  where
    try' e c = runExceptT e >>= either (hPutStrLn stderr) c
