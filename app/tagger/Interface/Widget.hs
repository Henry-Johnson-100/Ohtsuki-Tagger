{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget (
  taggerApplicationUI,
) where

import Data.Event
import Data.Model
import Interface.Widget.Internal
import Monomer

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerWidget
taggerApplicationUI _ m =
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
    $ vsplit_
      [splitIgnoreChildResize True, splitHandleSize 10]
      ( flip styleBasic [borderB 1 black] $ focusedFileWidget m
      , hsplit_
          [splitIgnoreChildResize True]
          ( flip styleBasic [borderT 1 black, borderR 1 black] $
              descriptorTreeWidget m
          , taggerInfoWidget m
          )
      )