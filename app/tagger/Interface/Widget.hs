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
    ]
    []
    $ vsplit_
      [splitIgnoreChildResize True, splitHandleSize 10]
      ( focusedFileWidget m
      , flip styleBasic [borderT 1 black] $
          hgrid_
            []
            [ descriptorTreeWidget m
            , fileSelectionOperationWidget m
            , fileSelectionWidget m
            ]
      )