{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget.Internal.Selection (
  widget,
) where

import Data.Model
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer

widget :: TaggerModel -> TaggerWidget
widget _ = label_ "The selection goes here." [resizeFactor (-1)]