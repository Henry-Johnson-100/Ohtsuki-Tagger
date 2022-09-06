module Interface.Widget.Internal.Core (
  styledButton,
  styledButton_,
  withStyleBasic,
  withStyleHover,
  withNodeKey,
  withNodeVisible,
) where

import Data.Event (TaggerEvent)
import Data.Model.Core (TaggerModel)
import Data.Text (Text)
import Interface.Theme (
  yuiLightPeach,
  yuiOrange,
  yuiPeach,
  yuiYellow,
 )
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  ButtonCfg,
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbStyleBasic (styleBasic),
  CmbStyleHover (styleHover),
  StyleState,
  WidgetNode,
  button,
  button_,
  nodeKey,
  nodeVisible,
 )

styledButton :: Text -> TaggerEvent -> TaggerWidget
styledButton t e =
  withStyleHover [bgColor yuiYellow, border 1 yuiOrange]
    . withStyleBasic [bgColor yuiLightPeach, border 1 yuiPeach]
    $ button t e

styledButton_ ::
  [ButtonCfg TaggerModel TaggerEvent] ->
  Text ->
  TaggerEvent ->
  TaggerWidget
styledButton_ opts t e =
  withStyleHover [bgColor yuiYellow, border 1 yuiOrange]
    . withStyleBasic [bgColor yuiLightPeach, border 1 yuiPeach]
    $ button_ t e opts

withStyleBasic ::
  [StyleState] ->
  WidgetNode TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent
withStyleBasic = flip styleBasic

withStyleHover ::
  [StyleState] ->
  WidgetNode TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent
withStyleHover = flip styleHover

withNodeVisible :: Bool -> TaggerWidget -> TaggerWidget
withNodeVisible = flip nodeVisible

withNodeKey :: Text -> TaggerWidget -> TaggerWidget
withNodeKey = flip nodeKey
