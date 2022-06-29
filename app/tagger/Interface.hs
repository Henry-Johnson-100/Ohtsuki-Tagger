module Interface (
  runTagger,
) where

import Control.Lens
import Data.Config
import Data.Event
import Data.Model
import Interface.Handler
import Interface.Theme
import Interface.Widget
import Monomer

runTagger :: TaggerModel -> IO ()
runTagger m = do
  themeConf <- themeConfig $ m ^. conf . styleConf
  startApp
    m
    taggerEventHandler
    taggerApplicationUI
    (themeConf ++ [appInitEvent TaggerInit])