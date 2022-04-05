{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.Text hiding (map)
import Database.Tagger.Access
import Database.Tagger.Type
import Monomer
import Monomer.Common.Lens
import Node.Base
import Type.Model

getAllFilesIO :: String -> IO [FileWithTags]
getAllFilesIO connString =
  connectThenRun connString $
    fetchAllFiles >>= fmap catMaybes . mapM getFileWithTags >>= liftIO . return

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler wenv node model event =
  case event of
    TaggerInit ->
      [ Model model,
        Task
          ( FileDbUpdate
              <$> (getAllFilesIO (model ^. connectionString))
          )
      ]
    FileDbUpdate fs -> [Model $ model & fileDb .~ fs]

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  WidgetNode TaggerModel TaggerEvent
taggerApplicationUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Tagger",
          spacer,
          label "Tagger",
          spacer,
          vstack (label <$> (map (pack . show) (model ^. fileDb)))
        ]
        `styleBasic` [padding 10]

taggerApplicationConfig :: [AppConfig TaggerEvent]
taggerApplicationConfig =
  [ appInitEvent TaggerInit
  ]
    ++ themeConfig

runTaggerWindow :: IO ()
runTaggerWindow =
  startApp
    (emptyTaggerModel "/home/monax/Repo/Haskell/TaggerLib/images.db.backup")
    taggerEventHandler
    taggerApplicationUI
    taggerApplicationConfig

main :: IO ()
main = runTaggerWindow
