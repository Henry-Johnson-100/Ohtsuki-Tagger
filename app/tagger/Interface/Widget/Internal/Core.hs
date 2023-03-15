module Interface.Widget.Internal.Core (
  styledButton_,
  withStyleBasic,
  withStyleHover,
  withNodeKey,
  withNodeVisible,
  withNodeHidden,
  defaultElementOpacity,
  defaultOpacityModulator,
  modulateOpacity,
) where

import Control.Lens ((&), (.~))
import Data.Event (TaggerEvent)
import Data.Model.Core (TaggerModel)
import Data.Text (Text)
import Interface.Theme (
  yuiLightPeach,
  yuiOrange,
  yuiPeach,
  yuiYellow,
 )
import Monomer (
  ButtonCfg,
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbStyleBasic (styleBasic),
  CmbStyleHover (styleHover),
  StyleState,
  WidgetNode,
  button_,
  nodeKey,
  nodeVisible,
 )
import Monomer.Graphics (Color)
import Monomer.Lens (HasA (a))

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

styledButton_ ::
  [ButtonCfg TaggerModel TaggerEvent] ->
  Text ->
  TaggerEvent ->
  TaggerWidget
styledButton_ opts t e =
  withStyleHover
    [ bgColor
        . modulateOpacity
          defaultElementOpacity
        $ yuiYellow
    , border 1
        . modulateOpacity
          defaultElementOpacity
        $ yuiOrange
    ]
    . withStyleBasic
      [ bgColor
          . modulateOpacity
            (defaultElementOpacity - defaultOpacityModulator)
          $ yuiLightPeach
      , border 1
          . modulateOpacity
            (defaultElementOpacity - defaultOpacityModulator)
          $ yuiPeach
      ]
    $ button_ t e opts

withStyleBasic ::
  [StyleState] ->
  WidgetNode TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent
withStyleBasic = flip styleBasic
{-# INLINE withStyleBasic #-}

withStyleHover ::
  [StyleState] ->
  WidgetNode TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent
withStyleHover = flip styleHover
{-# INLINE withStyleHover #-}

{- |
 Makes the given node visible when the predicate is true.
-}
withNodeVisible :: Bool -> TaggerWidget -> TaggerWidget
withNodeVisible = flip nodeVisible
{-# INLINE withNodeVisible #-}

{- |
 Hides the given node when the predicate is true.
-}
withNodeHidden :: Bool -> TaggerWidget -> TaggerWidget
withNodeHidden = withNodeVisible . not
{-# INLINE withNodeHidden #-}

withNodeKey :: Text -> TaggerWidget -> TaggerWidget
withNodeKey = flip nodeKey
{-# INLINE withNodeKey #-}

defaultElementOpacity :: Double
defaultElementOpacity = 0.5
{-# INLINE defaultElementOpacity #-}

defaultOpacityModulator :: Double
defaultOpacityModulator = 0.35
{-# INLINE defaultOpacityModulator #-}

modulateOpacity :: Double -> Color -> Color
modulateOpacity d c = c & a .~ d
{-# INLINE modulateOpacity #-}