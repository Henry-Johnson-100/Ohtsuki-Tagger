module Interface (
  runTagger,
) where

import Data.Event (TaggerEvent (TaggerInit))
import Data.Model.Core (TaggerModel)
import Interface.Handler (taggerEventHandler)
import Interface.Theme (themeConfig)
import Interface.Widget (taggerApplicationUI)
import Monomer (appInitEvent, startApp)

runTagger :: TaggerModel -> IO ()
runTagger m = do
  themeConf <- themeConfig
  startApp
    m
    taggerEventHandler
    taggerApplicationUI
    (themeConf ++ [appInitEvent TaggerInit])