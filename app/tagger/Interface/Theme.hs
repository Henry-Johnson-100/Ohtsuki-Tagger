{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Theme (
  module Interface.Theme,
) where

import Control.Lens
import Data.Config
import Monomer hiding (icon)
import Monomer.Core.Themes.BaseTheme

themeConfig :: StyleConfig -> [AppConfig e]
themeConfig cfg =
  [ appWindowTitle "Tagger"
  , appWindowState $
      if cfg ^. window . maximize
        then MainWindowMaximized
        else
          MainWindowNormal
            ( fromIntegral $ cfg ^. window . sizeX
            , fromIntegral $ cfg ^. window . sizeY
            )
  , appScaleFactor $ cfg ^. window . scalingFactor
  , appTheme yuiTheme
  -- , appFontDef "Regular" (cfg ^. font . regular)
  -- , appFontDef "Thin" (cfg ^. font . thin)
  -- , appFontDef "Bold" (cfg ^. font . bold)
  ]
    ++ maybe [] ((: []) . appWindowIcon) (cfg ^. window . icon)

yuiTheme :: Theme
yuiTheme =
  baseTheme
    lightThemeColors
      { clearColor = yuiLightPeach
      , sectionColor = yuiRed
      , -- btn
        btnBgBasic = yuiLightPeach
      , btnBgFocus = yuiYellow
      , btnFocusBorder = yuiOrange
      , btnBgHover = yuiPeach
      , btnBgActive = yuiOrange
      , -- input
        inputBgBasic = yuiLightPeach
      , inputBgFocus = yuiYellow
      , inputFocusBorder = yuiOrange
      , -- input selected
        inputSelFocus = yuiOrange
      , inputSelBasic = yuiYellow
      , -- dialog
        dialogBg = yuiLightPeach
      , -- sl and dropdowns
        slMainBg = yuiLightPeach
      , -- sl normal
        slNormalBgHover = yuiYellow
      , slNormalFocusBorder = yuiOrange
      , -- sl selected
        slSelectedBgBasic = yuiPeach
      , slSelectedBgHover = yuiOrange
      , slSelectedFocusBorder = yuiRed
      }

yuiPeach :: Color
yuiPeach = rgbHex "#FFECDE"

yuiLightPeach :: Color
yuiLightPeach = rgbHex "#FFF9F6"

yuiYellow :: Color
yuiYellow = rgbHex "#FFE29E"

yuiRed :: Color
yuiRed = rgbHex "#E5444A"

yuiOrange :: Color
yuiOrange = rgbHex "#FF8A44"

yuiBlue :: Color
yuiBlue = rgbHex "#3554A0"