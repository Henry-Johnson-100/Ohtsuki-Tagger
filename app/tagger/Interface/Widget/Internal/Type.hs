module Interface.Widget.Internal.Type (
  TaggerWidget,
) where

import Data.Event (TaggerEvent)
import Data.Model (TaggerModel)
import Monomer (WidgetNode)

type TaggerWidget = WidgetNode TaggerModel TaggerEvent