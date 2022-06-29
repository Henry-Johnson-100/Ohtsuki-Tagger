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
  keystroke_ [("Ctrl-R", RefreshUI)] [ignoreChildrenEvts] $
    vsplit_ [splitIgnoreChildResize True] (focusedFileWidget m, descriptorTreeWidget m)