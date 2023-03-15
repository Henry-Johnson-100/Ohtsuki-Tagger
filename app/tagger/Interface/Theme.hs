{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Theme (
  module Interface.Theme,
) where

import Config (TaggerConfig (taggerConfigScaleFactor), getOptConf)
import Control.Monad (join)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Event (TaggerEvent (CloseConnection))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Monomer (
  AppConfig,
  Color,
  MainWindowState (MainWindowMaximized),
  Theme,
  appDisposeEvent,
  appFontDef,
  appScaleFactor,
  appTheme,
  appWindowIcon,
  appWindowState,
  appWindowTitle,
  black,
  lightThemeColors,
  rgbHex,
  white,
 )
import Monomer.Core.Themes.BaseTheme (
  BaseThemeColors (
    btnBgActive,
    btnBgBasic,
    btnBgFocus,
    btnBgHover,
    btnFocusBorder,
    clearColor,
    dialogBg,
    inputBgBasic,
    inputBgFocus,
    inputFocusBorder,
    inputSelBasic,
    inputSelFocus,
    sectionColor,
    slMainBg,
    slNormalBgHover,
    slNormalFocusBorder,
    slSelectedBgBasic,
    slSelectedBgHover,
    slSelectedFocusBorder
  ),
  baseTheme,
 )
import qualified Paths_tagger as PT
import System.Directory (makeAbsolute)

themeConfig :: IO [AppConfig TaggerEvent]
themeConfig = do
  defaultThinFont <-
    T.pack
      <$> (makeAbsolute =<< PT.getDataFileName "resources/iosevka_thin.ttf")
  defaultRegularFont <-
    T.pack
      <$> (makeAbsolute =<< PT.getDataFileName "resources/iosevka_regular.ttf")
  defaultBoldFont <-
    T.pack
      <$> (makeAbsolute =<< PT.getDataFileName "resources/iosevka_bold.ttf")
  dataIcon <-
    T.pack
      <$> (makeAbsolute =<< PT.getDataFileName "resources/Yui_signature_SS.bmp")
  maybeDefaultScaleFactor <- join <$> runMaybeT (taggerConfigScaleFactor <$> getOptConf)
  return
    [ appWindowTitle "Tagger"
    , appWindowState MainWindowMaximized
    , appScaleFactor $ fromMaybe 1.0 maybeDefaultScaleFactor
    , appTheme yuiTheme
    , appFontDef "Regular" defaultRegularFont
    , appFontDef "Thin" defaultThinFont
    , appFontDef "Bold" defaultBoldFont
    , appWindowIcon dataIcon
    , appDisposeEvent CloseConnection
    ]

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

yuiBlack :: Color
yuiBlack = black

yuiWhite :: Color
yuiWhite = white