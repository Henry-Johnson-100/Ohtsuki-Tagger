module Interface (
  runTagger,
) where

import Control.Lens
import Data.Config
import Data.Model
import Interface.Handler
import Interface.Theme
import Interface.Widget
import Monomer

runTagger :: TaggerConfig -> TaggerModel -> IO ()
runTagger c m =
  startApp
    m
    taggerEventHandler
    taggerApplicationUI
    (themeConfig $ c ^. styleConf)