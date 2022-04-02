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
    appFontDef "Regular" "app/Config/Resources/iosevka_medium.ttf"
  ]