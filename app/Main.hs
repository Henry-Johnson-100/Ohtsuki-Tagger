{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Trans.Except (runExceptT)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, close, open)
import Database.Tagger.Access (activateForeignKeyPragma)
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
taggerApplicationUI wenv model =
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
  configPath <- getConfigPath
  let configExcept = getConfig configPath
  try' configExcept $ \config -> do
    rawArgs <- getArgs
    let opts@(TaggerOpts fls _ _) = getTaggerOpt rawArgs
    hasOptErrors <- showOptErrors opts
    unless hasOptErrors $ do
      if nullOpts opts
        then do
          runTaggerWindow config
        else do
          when (Version `elem` fls) (putStrLn taggerVersion)
          when
            (hasQueryFlag opts)
            ( do
                c <- open . T.unpack $ config ^. dbconf . dbconfPath
                cliQuery c . fromJust . getQuery $ opts
                close c
            )
  where
    try' e c = runExceptT e >>= either (hPutStrLn stderr) c
