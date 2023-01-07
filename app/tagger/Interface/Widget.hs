{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

module Interface.Widget (
  taggerApplicationUI,
) where

import Control.Lens ((%~), (&), (.~), (^.))
import Data.Event (
  FileSelectionEvent (
    ClearSelection,
    CycleNextFile,
    CyclePrevFile
  ),
  TaggerEvent (
    DoFileSelectionEvent,
    RefreshUI,
    ToggleQueryEditMode,
    ToggleVisibilityLabel
  ),
  anonymousEvent,
 )
import Data.Model.Core (
  TaggerModel,
  createPositioningModel,
  defaultFileDetailAndDescriptorTreePositioningModel,
  defaultSelectionAndQueryPositioningModel,
 )
import Data.Model.Lens (
  HasExpression (expression),
  HasFileDetailAndDescriptorTreePosH (fileDetailAndDescriptorTreePosH),
  HasFileDetailAndDescriptorTreePosV (fileDetailAndDescriptorTreePosV),
  HasFileSelectionModel (fileSelectionModel),
  HasPositioningModel (positioningModel),
  HasQueryEditMode (queryEditMode),
  HasQueryModel (queryModel),
  HasSelectionAndQueryPosH (selectionAndQueryPosH),
  HasSelectionAndQueryPosV (selectionAndQueryPosV),
  HasVisibilityModel (visibilityModel),
  TaggerLens (TaggerLens),
 )
import Data.Model.Shared.Core (
  Visibility (VisibilityLabel),
  hasVis,
 )
import Data.Text (Text)
import Interface.Theme (yuiLightPeach)
import Interface.Widget.Internal.Core (
  defaultElementOpacity,
  withNodeHidden,
  withNodeVisible,
  withStyleBasic,
 )
import qualified Interface.Widget.Internal.DescriptorTree as DescriptorTree
import qualified Interface.Widget.Internal.FileDetail as FileDetail
import qualified Interface.Widget.Internal.FilePreview as FilePreview
import qualified Interface.Widget.Internal.Query as Query
import Interface.Widget.Internal.Query.QueryBuilder (
  expressionWidget,
  queryEditorTextFieldKey,
 )
import qualified Interface.Widget.Internal.Selection as Selection
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  CmbBgColor (bgColor),
  CmbBorderB (borderB),
  CmbBorderL (borderL),
  CmbBorderR (borderR),
  CmbBorderT (borderT),
  CmbIgnoreEmptyArea (ignoreEmptyArea),
  CmbMaxWidth (maxWidth),
  CmbPaddingB (paddingB),
  CmbPaddingT (paddingT),
  CmbResizeFactor (resizeFactor),
  EventResponse (Event, Model, SetFocusOnKey),
  WidgetEnv,
  WidgetKey (WidgetKey),
  black,
  box_,
  expandContent,
  hsplit_,
  keystroke_,
  onlyTopActive,
  onlyTopActive_,
  spacer_,
  splitHandlePos,
  splitIgnoreChildResize,
  vsplit_,
  zstack_,
 )
import Monomer.Graphics.Lens (HasA (a))

taggerApplicationUI ::
  WidgetEnv TaggerModel TaggerEvent ->
  TaggerModel ->
  TaggerWidget
taggerApplicationUI _ m =
  globalKeystrokes m . zstack_ [onlyTopActive] $
    [ baseZStack
        m
        [ selectionQueryLayer m
        , fileDetailAndDescriptorTreeLayer m
        ]
    , queryEditorPane m
    ]

queryEditorPane :: TaggerModel -> TaggerWidget
queryEditorPane m =
  withNodeVisible (m ^. queryEditMode)
    . withStyleBasic [bgColor yuiLightPeach]
    . box_ [expandContent]
    . expressionWidget
    $ m
      ^. fileSelectionModel
        . queryModel
        . expression

globalWidgetHideLabel :: Text
globalWidgetHideLabel = "global-widget-hide"

baseZStack :: TaggerModel -> [TaggerWidget] -> TaggerWidget
baseZStack m ws = zstack_ [onlyTopActive_ False] (FilePreview.widget m : ws)

fileDetailDescriptorTreeHide :: Text
fileDetailDescriptorTreeHide = "file-detail-and-descriptor-tree-vis"

fileDetailAndDescriptorTreeLayer :: TaggerModel -> TaggerWidget
fileDetailAndDescriptorTreeLayer m =
  withNodeHidden
    ( or
        ( hasVis (m ^. visibilityModel) . VisibilityLabel
            <$> [fileDetailDescriptorTreeHide, globalWidgetHideLabel]
        )
    )
    $ hsplit_
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

selectionQueryLayer :: TaggerModel -> TaggerWidget
selectionQueryLayer m =
  withNodeHidden
    ( or
        ( hasVis (m ^. visibilityModel) . VisibilityLabel
            <$> [selectionQueryHideLabel, globalWidgetHideLabel]
        )
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

globalKeystrokes :: TaggerModel -> TaggerWidget -> TaggerWidget
globalKeystrokes m =
  keystroke_
    [ ("Ctrl-r", RefreshUI)
    , ("Ctrl-i", DoFileSelectionEvent CycleNextFile)
    , ("Ctrl-k", DoFileSelectionEvent CyclePrevFile)
    ,
      ( "Ctrl-t"
      , anonymousEvent $
          if (m ^. visibilityModel) `hasVis` VisibilityLabel fileDetailDescriptorTreeHide
            then
              [ Event $
                  ToggleVisibilityLabel
                    (TaggerLens visibilityModel)
                    fileDetailDescriptorTreeHide
              , SetFocusOnKey . WidgetKey $ FileDetail.tagTextNodeKey
              ]
            else [SetFocusOnKey . WidgetKey $ FileDetail.tagTextNodeKey]
      )
    ,
      ( "Ctrl-f"
      , anonymousEvent $
          let setFocusEvent =
                SetFocusOnKey . WidgetKey $
                  if m ^. queryEditMode
                    then queryEditorTextFieldKey
                    else Query.queryTextFieldKey
           in if (m ^. visibilityModel) `hasVis` VisibilityLabel selectionQueryHideLabel
                then
                  [ Event $
                      ToggleVisibilityLabel
                        (TaggerLens visibilityModel)
                        selectionQueryHideLabel
                  , setFocusEvent
                  ]
                else [setFocusEvent]
      )
    , ("Ctrl-d", ToggleQueryEditMode)
    ,
      ( "Ctrl-h"
      , anonymousEvent
          [ Model $ m & positioningModel .~ createPositioningModel
          , Event $
              ToggleVisibilityLabel
                (TaggerLens visibilityModel)
                globalWidgetHideLabel
          ]
      )
    ,
      ( "Ctrl-q"
      , anonymousEvent
          [ Model $
              m & positioningModel
                %~ defaultSelectionAndQueryPositioningModel
          , Event $
              ToggleVisibilityLabel
                (TaggerLens visibilityModel)
                selectionQueryHideLabel
          ]
      )
    ,
      ( "Ctrl-e"
      , anonymousEvent
          [ Model $
              m & positioningModel
                %~ defaultFileDetailAndDescriptorTreePositioningModel
          , Event $
              ToggleVisibilityLabel
                (TaggerLens visibilityModel)
                fileDetailDescriptorTreeHide
          ]
      )
    ]
    []

bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap f g (x, y) = (f x, g y)