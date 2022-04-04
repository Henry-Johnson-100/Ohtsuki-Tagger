{-# LANGUAGE OverloadedStrings #-}

module Node.FileDb
  ( FileDbModel (..),
    FileDbEvent (..),
    fileDbUi,
    fileDbEventHandler,
    fileDbConfig,
  )
where

import Data.Text
import Monomer

data FileDbModel = FileDbModel deriving (Show, Eq)

data FileDbEvent = FileDbInit deriving (Show, Eq)

fileDbEventHandler ::
  WidgetEnv FileDbModel FileDbEvent ->
  WidgetNode FileDbModel FileDbEvent ->
  FileDbModel ->
  FileDbEvent ->
  [AppEventResponse FileDbModel FileDbEvent]
fileDbEventHandler wenv node model event =
  case event of
    FileDbInit -> []

fileDbUi ::
  WidgetEnv FileDbModel FileDbEvent ->
  FileDbModel ->
  WidgetNode FileDbModel FileDbEvent
fileDbUi wenv model =
  widgetTree
  where
    widgetTree =
      label "FileDb"

fileDbConfig :: [AppConfig FileDbEvent]
fileDbConfig = [appInitEvent FileDbInit]