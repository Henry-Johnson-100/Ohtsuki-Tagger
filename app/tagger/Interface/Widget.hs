{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Interface.Widget (
  taggerApplicationUI,
) where

import Control.Lens hiding (bimap, both)
import Data.Event
import Data.Model
import Data.Model.Shared.Core
import Data.Text (Text)
import Interface.Theme
import Interface.Widget.Internal
import Interface.Widget.Internal.Core
import qualified Interface.Widget.Internal.DescriptorTree as DescriptorTree
import qualified Interface.Widget.Internal.FileDetail as FileDetail
import qualified Interface.Widget.Internal.FilePreview as FilePreview
import qualified Interface.Widget.Internal.Query as Query
import qualified Interface.Widget.Internal.Selection as Selection
import Monomer
import Monomer.Graphics.Lens (HasA (a))

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerWidget
taggerApplicationUI _ m =
  globalKeystrokes
    . baseZStack m
    $ [ selectionQueryLayer m
      , fileDetailAndDescriptorTreeLayer m
      ]

baseZStack :: TaggerModel -> [TaggerWidget] -> TaggerWidget
baseZStack m ws = zstack_ [onlyTopActive_ False] (FilePreview.widget m : ws)

fileDetailDescriptorTreeHide :: Text
fileDetailDescriptorTreeHide = "file-detail-and-descriptor-tree-vis"

fileDetailAndDescriptorTreeLayer :: TaggerModel -> TaggerWidget
fileDetailAndDescriptorTreeLayer m =
  withNodeHidden ((m ^. visibilityModel) `hasVis` VisibilityLabel fileDetailDescriptorTreeHide) $
    hsplit_
      [ splitIgnoreChildResize True
      , splitHandlePos
          (positioningModel . fileDetailAndDescriptorTreePosH)
      ]
      ( withStyleBasic [maxWidth 10000, borderR 1 $ black & a .~ 0.10]
          . box_ [ignoreEmptyArea]
          . withStyleBasic [maxWidth 0]
          $ spacer_ [resizeFactor (-1)]
      , withStyleBasic [bgColor $ yuiLightPeach & a .~ defaultElementOpacity]
          . vsplit_
            [ splitIgnoreChildResize True
            , splitHandlePos (positioningModel . fileDetailAndDescriptorTreePosV)
            ]
          . bimap
            (withStyleBasic [borderB 1 black, paddingB 10])
            (withStyleBasic [borderT 1 black, paddingT 3])
          $ (FileDetail.widget m, DescriptorTree.widget m)
      )

selectionQueryHideLabel :: Text
selectionQueryHideLabel = "selection-and-query-layer-hide"

selectionQueryHideEvent :: TaggerEvent
selectionQueryHideEvent = ToggleMainVisibility selectionQueryHideLabel

selectionQueryLayer :: TaggerModel -> TaggerWidget
selectionQueryLayer m =
  withNodeHidden
    ( (m ^. visibilityModel)
        `hasVis` VisibilityLabel selectionQueryHideLabel
    )
    $ hsplit_
      [ splitIgnoreChildResize True
      , splitHandlePos
          (positioningModel . selectionAndQueryPosH)
      ]
      ( withStyleBasic
          [bgColor $ yuiLightPeach & a .~ defaultElementOpacity]
          . vsplit_
            [ splitIgnoreChildResize True
            , splitHandlePos
                (positioningModel . selectionAndQueryPosV)
            ]
          . bimap
            (withStyleBasic [borderB 1 black, paddingB 10])
            (withStyleBasic [borderT 1 black, paddingT 3])
          $ (Query.widget m, Selection.widget m)
      , withStyleBasic [maxWidth 10000, borderL 1 $ black & a .~ 0.10]
          . box_ [ignoreEmptyArea]
          . withStyleBasic [maxWidth 0]
          $ spacer_ [resizeFactor (-1)]
      )

globalKeystrokes :: TaggerWidget -> TaggerWidget
globalKeystrokes =
  keystroke_
    [ ("Ctrl-r", RefreshUI)
    , ("Ctrl-i", DoFileSelectionEvent CycleNextFile)
    , ("Ctrl-k", DoFileSelectionEvent CyclePrevFile)
    , ("Ctrl-t", FocusTagTextField)
    ,
      ( "Ctrl-Shift-t"
      , DoFocusedFileEvent
          ( ToggleFocusedFilePaneVisibility
              zstackTaggingWidgetVis
          )
      )
    , ("Ctrl-f", FocusQueryTextField)
    ,
      ( "Ctrl-Shift-f"
      , DoFocusedFileEvent
          (ToggleFocusedFilePaneVisibility zstackQueryWidgetVis)
      )
    , ("Ctrl-g", DoFileSelectionEvent CycleNextSetOp)
    , ("Ctrl-Shift-g", DoFileSelectionEvent CyclePrevSetOp)
    , ("Ctrl-u", DoFileSelectionEvent ClearSelection)
    , -- , ("Ctrl-h", ToggleMainVisibility hidePossibleUIVis)
      ("Ctrl-q", selectionQueryHideEvent)
    , ("Ctrl-e", ToggleMainVisibility fileDetailDescriptorTreeHide)
    ]
    []

bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap f g (x, y) = (f x, g y)