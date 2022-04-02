{-# LANGUAGE OverloadedStrings #-}

module Config.Window
  ( themeConfig,
  )
where

import Data.Text
import Monomer (AppConfig, appFontDef, appTheme, lightTheme)

themeConfig :: [AppConfig e]
themeConfig =
  [ appTheme lightTheme,
    appFontDef "Regular" "iosevka_medium.ttf"
  ]