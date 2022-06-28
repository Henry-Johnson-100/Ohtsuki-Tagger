{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Theme (
  module Interface.Theme,
) where

import Control.Lens
import Data.Config
import Data.Maybe
import qualified Data.Text as T
import Monomer hiding (icon)
import Monomer.Core.Themes.BaseTheme
import qualified Paths_tagger as PT
import System.Directory

themeConfig :: StyleConfig -> IO [AppConfig e]
themeConfig cfg = do
  defaultThinFont <- T.pack <$> (makeAbsolute =<< PT.getDataFileName "iosevka_thin.ttf")
  defaultRegularFont <-
    T.pack
      <$> (makeAbsolute =<< PT.getDataFileName "iosevka_regular.ttf")
  defaultBoldFont <- T.pack <$> (makeAbsolute =<< PT.getDataFileName "iosevka_bold.ttf")
  dataIcon <- T.pack <$> (makeAbsolute =<< PT.getDataFileName "Yui_signature_SS.bmp")
  return
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
    , appFontDef "Regular" . fromMaybe defaultRegularFont $ cfg ^. font . regular
    , appFontDef "Thin" . fromMaybe defaultThinFont $ cfg ^. font . thin
    , appFontDef "Bold" . fromMaybe defaultBoldFont $ cfg ^. font . bold
    , appWindowIcon dataIcon
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