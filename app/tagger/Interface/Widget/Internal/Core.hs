module Interface.Widget.Internal.Core (
  styledButton_,
  withStyleBasic,
  withStyleHover,
  withNodeKey,
  withNodeVisible,
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
import Interface.Widget.Internal.Type (TaggerWidget)
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
import Monomer.Lens (HasA (a))

styledButton_ ::
  [ButtonCfg TaggerModel TaggerEvent] ->
  Text ->
  TaggerEvent ->
  TaggerWidget
styledButton_ opts t e =
  withStyleHover
    [ bgColor $
        yuiYellow & a
          .~ modulateOpacity
            negate
            defaultOpacityModulator
            defaultElementOpacity
    , border 1 yuiOrange
    ]
    . withStyleBasic
      [ bgColor $
          yuiLightPeach & a
            .~ modulateOpacity
              negate
              defaultOpacityModulator
              defaultElementOpacity
      , border 1 yuiPeach
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

withNodeVisible :: Bool -> TaggerWidget -> TaggerWidget
withNodeVisible = flip nodeVisible
{-# INLINE withNodeVisible #-}

withNodeKey :: Text -> TaggerWidget -> TaggerWidget
withNodeKey = flip nodeKey
{-# INLINE withNodeKey #-}

defaultElementOpacity :: Double
defaultElementOpacity = 0.5
{-# INLINE defaultElementOpacity #-}

defaultOpacityModulator :: Double
defaultOpacityModulator = 0.1
{-# INLINE defaultOpacityModulator #-}

modulateOpacity :: (Double -> Double) -> Double -> Double -> Double
modulateOpacity f d n = f d + n
{-# INLINE modulateOpacity #-}