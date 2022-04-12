{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use ?~" #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Char
import Data.List
import Data.Maybe
import Data.Text hiding (head, map, take)
import Database.SQLite.Simple (Only (Only), close, execute_, open, query_)
import Database.Tagger.Access (Connection)
import Database.Tagger.Type
import Event.Handler
import Event.Task
import Monomer
import Monomer.Common.Lens
import Node.Application
import System.Process
import Type.Model

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
              (fileSinglePreviewWidget model)
            ]
        ]

taggerApplicationConfig :: [AppConfig TaggerEvent]
taggerApplicationConfig =
  [ appInitEvent TaggerInit
  ]
    ++ themeConfig

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