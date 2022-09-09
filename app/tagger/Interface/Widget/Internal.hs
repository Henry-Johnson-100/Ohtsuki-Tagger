{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget.Internal (
  hidePossibleUIVis,
  zstackTaggingWidgetVis,
  zstackQueryWidgetVis,
) where

import Data.Text (Text)

-- # TODO need to change the handlers and how some other visibiltiies work before I
-- I can delete / change / move this value.
hidePossibleUIVis :: Text
hidePossibleUIVis = "hide-possible-elements"

-- # TODO need to change the handlers and how some other visibiltiies work before I
-- I can delete / change / move this value.
zstackTaggingWidgetVis :: Text
zstackTaggingWidgetVis = "show-tag-field"

-- # TODO need to change the handlers and how some other visibiltiies work before I
-- I can delete / change / move this value.
zstackQueryWidgetVis :: Text
zstackQueryWidgetVis = "show-query-field"

-- # TODO add these buttons to the main widget page tabun
-- zstackNextImage =
--   withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
--     styledButton_ [resizeFactor (-1)] "↑" (DoFileSelectionEvent CycleNextFile)
-- zstackPrevImage =
--   withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
--     styledButton_ [resizeFactor (-1)] "↓" (DoFileSelectionEvent CyclePrevFile)
