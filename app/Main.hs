{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use ?~" #-}

module Main where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe
import Data.Text hiding (head, map, take)
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

doSetAction :: FileSetArithmetic -> [FileWithTags] -> [FileWithTags] -> [FileWithTags]
doSetAction a s o =
  case a of
    Union -> s `union` o
    Intersect -> s `intersect` o
    Diff -> s \\ o

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler wenv node model event =
  case event of
    TaggerInit ->
      [ Task
          ( FileDbUpdate
              <$> (getAllFilesIO (model ^. connectionString))
          ),
        Model $
          model
            & fileSingle
              .~ (Just (FileWithTags (File "/home/monax/Pictures/dog.jpg") []))
      ]
    FileDbUpdate fs -> [Model $ model & fileDb .~ fs]
    FileSinglePut i -> [Model $ model & fileSingle .~ (Just i)]
    FileSetArithmetic a -> [Model $ model & fileSetArithmetic .~ a]
    FileSelectionUpdate ts ->
      [ Model $
          model & fileSelection
            .~ (doSetAction (model ^. fileSetArithmetic) (model ^. fileSelection) ts)
      ]

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
          fileDbWidget $ take 10 (model ^. fileDb),
          fileSinglePreviewWidget model
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
