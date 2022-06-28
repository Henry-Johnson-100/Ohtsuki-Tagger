{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget.Internal (
  TaggerWidget,
  descriptorTreeWidget,
) where

import Data.Event
import Data.Model
import Monomer

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

descriptorTreeWidget :: TaggerWidget
descriptorTreeWidget = vstack [descriptorTreeRefreshBothButton]

descriptorTreeRefreshBothButton :: TaggerWidget
descriptorTreeRefreshBothButton =
  button
    "Refresh"
    (DoDescriptorTreeEvent RefreshBothDescriptorTrees)