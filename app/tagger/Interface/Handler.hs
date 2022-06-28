module Interface.Handler (
  taggerEventHandler,
) where

import Data.Event
import Data.Model
import Monomer

taggerEventHandler ::
  WidgetEnv TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerEvent ->
  [AppEventResponse TaggerModel TaggerEvent]
taggerEventHandler _ _ _ event =
  case event of
    TogglePreviewFocus -> []
    ToggleTagMode -> []
    CloseConnection -> []
