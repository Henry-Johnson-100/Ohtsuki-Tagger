{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Interface.Widget.Internal.Query (
  queryTextFieldKey,
  widget,
) where

import Control.Lens
import Data.Event
import Data.Model
import Data.Model.Shared
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Interface.Theme
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer
import Monomer.Graphics.Lens

widget :: TaggerModel -> TaggerWidget
widget m =
  visibilities m
    . alignment
    . container
    $ queryTextField

queryTextField :: TaggerWidget
queryTextField =
  keystroke_
    [ ("Enter", DoFileSelectionEvent Query)
    , ("Up", DoFileSelectionEvent NextQueryHist)
    , ("Down", DoFileSelectionEvent PrevQueryHist)
    ]
    []
    . dropTarget_
      (DoFileSelectionEvent . AppendQueryText . descriptor . concreteTagDescriptor)
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      (DoFileSelectionEvent . AppendQueryText . filePath)
      [dropTargetStyle [border 1 yuiOrange]]
    . dropTarget_
      (DoFileSelectionEvent . AppendQueryText . descriptor)
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey queryTextFieldKey
    . withStyleBasic [bgColor (yuiLightPeach & a .~ defaultElementOpacity)]
    $ textField_
      (fileSelectionModel . queryText)
      [ onChange
          ( \t ->
              if T.null . T.strip $ t
                then DoFileSelectionEvent ResetQueryHistIndex
                else IOEvent ()
          )
      ]

alignment :: TaggerWidget -> TaggerWidget
alignment w = vgrid_ [] [w, spacer_ [resizeFactor (-1)]]

container :: TaggerWidget -> TaggerWidget
container w =
  box_ [alignLeft, alignBottom, ignoreEmptyArea]
    . hsplit_ [splitIgnoreChildResize True, splitHandlePosV 0.25]
    . (,withStyleBasic [maxWidth 10000, borderL 1 (black & a .~ 0.3)] $
          spacer_ [resizeFactor (-1)])
    . withStyleBasic [borderT 1 black, borderB 1 black, borderR 1 black]
    . vstack_ []
    $ [ withStyleBasic [paddingL 15] $ label_ "Query" [resizeFactor (-1)]
      , w
      ]

visibilities :: TaggerModel -> TaggerWidget -> TaggerWidget
visibilities = withNodeVisible . isVisible
{-# INLINE visibilities #-}

isVisible :: TaggerModel -> Bool
isVisible m =
  (m ^. focusedFileModel . focusedFileVis)
    `hasVis` VisibilityLabel zstackQueryWidgetVis

zstackQueryWidgetVis :: Text
zstackQueryWidgetVis = "show-query-field"
{-# INLINE zstackQueryWidgetVis #-}

queryTextFieldKey :: Text
queryTextFieldKey = "queryTextField"
{-# INLINE queryTextFieldKey #-}