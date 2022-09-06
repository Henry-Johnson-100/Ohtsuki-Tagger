{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget (
  taggerApplicationUI,
) where

import Data.Event
import Data.Model
import Interface.Widget.Internal
import qualified Interface.Widget.Internal.FilePreview as FilePreview
import qualified Interface.Widget.Internal.Query as Query
import Monomer

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerWidget
taggerApplicationUI _ m =
  globalKeystrokes
    . baseZStack m
    $ [Query.widget m]

baseZStack :: TaggerModel -> [TaggerWidget] -> TaggerWidget
baseZStack m ws = zstack_ [onlyTopActive_ False] (FilePreview.widget m : ws)

globalKeystrokes :: TaggerWidget -> TaggerWidget
globalKeystrokes =
  keystroke_
    [ ("Ctrl-r", RefreshUI)
    , ("Ctrl-i", DoFileSelectionEvent CycleNextFile)
    , ("Ctrl-k", DoFileSelectionEvent CyclePrevFile)
    , ("Ctrl-t", FocusTagTextField)
    ,
      ( "Ctrl-Shift-t"
      , DoFocusedFileEvent
          ( ToggleFocusedFilePaneVisibility
              zstackTaggingWidgetVis
          )
      )
    , ("Ctrl-f", FocusQueryTextField)
    ,
      ( "Ctrl-Shift-f"
      , DoFocusedFileEvent
          (ToggleFocusedFilePaneVisibility zstackQueryWidgetVis)
      )
    , ("Ctrl-g", DoFileSelectionEvent CycleNextSetOp)
    , ("Ctrl-Shift-g", DoFileSelectionEvent CyclePrevSetOp)
    , ("Ctrl-u", DoFileSelectionEvent ClearSelection)
    , ("Ctrl-h", ToggleMainVisibility hidePossibleUIVis)
    ]
    []