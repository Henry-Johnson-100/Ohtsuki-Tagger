{-# LANGUAGE OverloadedStrings #-}

module Node.Base
  ( themeConfig,
  )
where

import Data.Text ()
import Monomer

themeConfig :: [AppConfig e]
themeConfig =
  [ appWindowTitle "Hello World",
    appTheme lightTheme,
    appFontDef "Regular" "./resources/iosevka_thin.ttf"
  ]