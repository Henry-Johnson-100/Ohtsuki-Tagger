{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Interface.Widget.Internal.DescriptorTree (widget) where

import Control.Lens ((&), (.~), (^.))
import Data.Event (
  DescriptorTreeEvent (
    CreateRelation,
    DeleteDescriptor,
    InsertDescriptor,
    RefreshBothDescriptorTrees,
    RequestFocusedNode,
    RequestFocusedNodeParent,
    ToggleDescriptorTreeVisibility,
    UpdateDescriptor
  ),
  TaggerEvent (DoDescriptorTreeEvent),
 )
import qualified Data.List as L
import Data.Model (
  HasDescriptorInfoMap (descriptorInfoMap),
  HasDescriptorIsMeta (descriptorIsMeta),
  HasDescriptorTreeModel (descriptorTreeModel),
  HasDescriptorTreeVis (descriptorTreeVis),
  HasFocusedNode (focusedNode),
  HasFocusedTree (focusedTree),
  HasNewDescriptorText (newDescriptorText),
  HasRenameText (renameText),
  HasUnrelated (unrelated),
  HasUnrelatedNode (unrelatedNode),
  TaggerModel,
  descriptorInfoAt,
 )
import Data.Model.Shared (Visibility (VisibilityLabel), hasVis)
import Data.Text (Text)
import Database.Tagger (Descriptor (Descriptor, descriptor))
import Interface.Theme (
  yuiBlue,
  yuiLightPeach,
  yuiOrange,
  yuiRed,
  yuiYellow,
 )
import Interface.Widget.Internal.Core (
  defaultElementOpacity,
  defaultOpacityModulator,
  modulateOpacity,
  styledButton_,
  withNodeKey,
  withNodeVisible,
  withStyleBasic,
  withStyleHover,
 )
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  CmbAlignBottom (alignBottom),
  CmbAlignLeft (alignLeft),
  CmbAlignMiddle (alignMiddle),
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbBorderL (borderL),
  CmbBorderR (borderR),
  CmbMergeRequired (mergeRequired),
  CmbResizeFactor (resizeFactor),
  CmbSizeReqUpdater (sizeReqUpdater),
  CmbTextColor (textColor),
  CmbTextLeft (textLeft),
  CmbWheelRate (wheelRate),
  black,
  box_,
  button,
  button_,
  draggable,
  dropTargetStyle,
  dropTarget_,
  expandContent,
  hsplit_,
  hstack_,
  keystroke_,
  label_,
  separatorLine,
  splitIgnoreChildResize,
  textField_,
  vscroll_,
  vstack_,
  white,
  zstack_,
 )
import Monomer.Core.Lens (fixed)
import Util (both)

widget :: TaggerModel -> TaggerWidget
widget m = descriptorTreeWidget m

descriptorTreeWidget :: TaggerModel -> TaggerWidget
descriptorTreeWidget m =
  withNodeKey
    "descriptorTree"
    mainPane
 where
  mainPane =
    vstack_
      []
      [ hstack_
          []
          [ mainPaneLeftButtonStack
          , hsplit_
              [ splitIgnoreChildResize True
              ]
              ( descriptorTreeFocusedNodeWidget m
              , descriptorTreeUnrelatedWidget m
              )
          ]
      ]
   where
    mainPaneLeftButtonStack =
      vstack_
        []
        [ descriptorTreeRefreshBothButton
        , descriptorTreeRequestParentButton
        , descriptorTreeFixedRequestButton "#META#"
        ]

descriptorTreeRequestParentButton :: TaggerWidget
descriptorTreeRequestParentButton =
  styledButton_
    [resizeFactor (-1)]
    "Up"
    (DoDescriptorTreeEvent RequestFocusedNodeParent)

descriptorTreeFocusedNodeWidget :: TaggerModel -> TaggerWidget
descriptorTreeFocusedNodeWidget m =
  box_
    [ expandContent
    , mergeRequired
        ( \_ ((^. descriptorTreeModel) -> dm1) ((^. descriptorTreeModel) -> dm2) ->
            dm1 /= dm2
        )
    ]
    . withStyleBasic [borderR 1 black]
    . createRelationDropTarget
    $ descriptorTreeFocusedNodeWidgetBody
 where
  descriptorTreeFocusedNodeWidgetBody :: TaggerWidget
  descriptorTreeFocusedNodeWidgetBody =
    vstack_
      []
      [ nodeHeader
      , separatorLine
      , focusedTreeLeafWidget
      ]

  focusedTreeLeafWidget :: TaggerWidget
  focusedTreeLeafWidget =
    let focusedDescriptors = {-L.nub?-} m ^. descriptorTreeModel . focusedTree
        metaDescriptors =
          L.sort
            . filter
              (descriptorIsMetaInInfoMap m)
            $ focusedDescriptors
        infraDescriptors =
          L.sort
            . filter
              (not . descriptorIsMetaInInfoMap m)
            $ focusedDescriptors
     in vscroll_ [wheelRate 50] . vstack_ [] $
          descriptorTreeLeaf m
            <$> (metaDescriptors ++ infraDescriptors)

  nodeHeader :: TaggerWidget
  nodeHeader =
    flip label_ [resizeFactor (-1)]
      . descriptor
      $ m ^. descriptorTreeModel . focusedNode

  createRelationDropTarget :: TaggerWidget -> TaggerWidget
  createRelationDropTarget =
    dropTarget_
      (DoDescriptorTreeEvent . CreateRelation (m ^. descriptorTreeModel . focusedNode))
      [dropTargetStyle [border 3 yuiBlue]]

descriptorTreeUnrelatedWidget :: TaggerModel -> TaggerWidget
descriptorTreeUnrelatedWidget m =
  box_
    [ expandContent
    , mergeRequired
        ( \_ ((^. descriptorTreeModel) -> dm1) ((^. descriptorTreeModel) -> dm2) ->
            dm1 /= dm2
        )
    ]
    . withStyleBasic [borderL 1 black]
    . createUnrelationDropTargetWidget
      $descriptorTreeUnrelatedWidgetBody
 where
  descriptorTreeUnrelatedWidgetBody :: TaggerWidget
  descriptorTreeUnrelatedWidgetBody =
    vstack_
      []
      [ label_ "Unrelated" [resizeFactor (-1)]
      , separatorLine
      , unrelatedTreeLeafWidget
      , descriptorManagementPane
      ]

  unrelatedTreeLeafWidget :: TaggerWidget
  unrelatedTreeLeafWidget =
    let unrelatedDescriptors = m ^. descriptorTreeModel . unrelated
        meta =
          L.sort
            . filter
              (descriptorIsMetaInInfoMap m)
            $ unrelatedDescriptors
        infra =
          L.sort
            . filter
              (not . descriptorIsMetaInInfoMap m)
            $ unrelatedDescriptors
     in vscroll_ [wheelRate 50] . vstack_ [] $ descriptorTreeLeaf m <$> (meta ++ infra)

  createUnrelationDropTargetWidget :: TaggerWidget -> TaggerWidget
  createUnrelationDropTargetWidget =
    dropTarget_
      (DoDescriptorTreeEvent . CreateRelation (m ^. descriptorTreeModel . unrelatedNode))
      [dropTargetStyle [border 3 yuiBlue]]

descriptorTreeRefreshBothButton :: TaggerWidget
descriptorTreeRefreshBothButton =
  styledButton_
    [resizeFactor (-1)]
    "Refresh"
    (DoDescriptorTreeEvent RefreshBothDescriptorTrees)

descriptorTreeLeaf :: TaggerModel -> Descriptor -> TaggerWidget
descriptorTreeLeaf
  model@((^. descriptorTreeModel . descriptorInfoMap) -> m)
  d@(Descriptor dk p) =
    let di = m ^. descriptorInfoAt (fromIntegral dk)
     in box_ [alignLeft] $
          zstack_
            []
            [ withNodeVisible
                ( not $
                    (model ^. descriptorTreeModel . descriptorTreeVis)
                      `hasVis` VisibilityLabel editDescriptorVis
                )
                $ mainDescriptorLeafPageWidget di
            , withNodeVisible
                ( (model ^. descriptorTreeModel . descriptorTreeVis)
                    `hasVis` VisibilityLabel editDescriptorVis
                )
                editDescriptorLeafPageWidget
            ]
   where
    mainDescriptorLeafPageWidget di =
      hstack_
        []
        [ draggable d
            . box_ [alignLeft]
            . withStyleHover
              [ bgColor
                  . modulateOpacity
                    defaultElementOpacity
                  $ yuiYellow
              , border 1
                  . modulateOpacity
                    defaultElementOpacity
                  $ yuiOrange
              ]
            . withStyleBasic
              [ textColor (if di ^. descriptorIsMeta then yuiBlue else black)
              , textLeft
              , bgColor
                  . modulateOpacity
                    0.0
                  $ yuiLightPeach
              , border 1 . modulateOpacity 0.0 $ yuiLightPeach
              ]
            $ button p (DoDescriptorTreeEvent (RequestFocusedNode p))
        ]
    editDescriptorLeafPageWidget =
      hstack_
        []
        [ box_ [alignLeft]
            . keystroke_
              [("Enter", DoDescriptorTreeEvent (UpdateDescriptor dk))]
              []
            . withStyleBasic
              [ bgColor
                  . modulateOpacity
                    (defaultElementOpacity - defaultOpacityModulator)
                  $ yuiLightPeach
              ]
            $ textField_
              ( descriptorTreeModel
                  . descriptorInfoMap
                  . descriptorInfoAt (fromIntegral dk)
                  . renameText
              )
              []
        , box_ [alignLeft]
            . withStyleHover
              [ bgColor
                  . modulateOpacity defaultElementOpacity $yuiRed
              , textColor white
              ]
            . withStyleBasic
              [ textColor yuiRed
              , bgColor
                  . modulateOpacity
                    (defaultElementOpacity - defaultOpacityModulator)
                  $ yuiLightPeach
              ]
            $ button_
              "Delete"
              ( DoDescriptorTreeEvent
                  (DeleteDescriptor d)
              )
              [resizeFactor (-1)]
        ]

descriptorTreeFixedRequestButton :: Text -> TaggerWidget
descriptorTreeFixedRequestButton t =
  styledButton_
    [resizeFactor (-1)]
    "Top"
    (DoDescriptorTreeEvent . RequestFocusedNode $ t)

descriptorIsMetaInInfoMap :: TaggerModel -> Descriptor -> Bool
descriptorIsMetaInInfoMap
  ((^. descriptorTreeModel . descriptorInfoMap) -> m)
  (Descriptor (fromIntegral -> dk) _) = m ^. descriptorInfoAt dk . descriptorIsMeta

descriptorManagementPane :: TaggerWidget
descriptorManagementPane =
  insertDescriptorWidget
 where
  insertDescriptorWidget :: TaggerWidget
  insertDescriptorWidget =
    keystroke_ [("Enter", DoDescriptorTreeEvent InsertDescriptor)] [] . hstack_ [] $
      [ editDescriptorLeafButton
      , insertButton
      , box_
          [ alignBottom
          , alignMiddle
          , sizeReqUpdater
              (\(xs, xy) -> both (& fixed .~ 0) (xs, xy))
          ]
          . withStyleBasic
            [ bgColor
                . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
                $ yuiLightPeach
            ]
          $ textField_ (descriptorTreeModel . newDescriptorText) []
      ]
   where
    editDescriptorLeafButton =
      styledButton_
        [resizeFactor (-1)]
        "Edit"
        (DoDescriptorTreeEvent . ToggleDescriptorTreeVisibility $ editDescriptorVis)
    insertButton =
      styledButton_
        [resizeFactor (-1)]
        "New"
        (DoDescriptorTreeEvent InsertDescriptor)

editDescriptorVis :: Text
editDescriptorVis = "edit-descriptor"