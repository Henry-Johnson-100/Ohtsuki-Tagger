{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Window.Main
  ( runWindow,
  )
where

import Control.Lens
import Window.Core

data AppModel = AppModel
  { _selectionFiles :: [Text]
  }
  deriving (Show, Eq)

data AppEvent = AppInit deriving (Show, Eq)

makeLensesWith abbreviatedFields ''AppModel

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt =
  case evt of
    AppInit -> []

winConfig :: [AppConfig AppEvent]
winConfig =
  [ appWindowTitle "Tagger Main",
    appInitEvent AppInit
  ]
    ++ themeConfig

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ label "Tagger Main Window",
          spacer
        ]
        `styleBasic` [padding 10]

runWindow :: IO ()
runWindow = startApp (AppModel []) handleEvent buildUI winConfig