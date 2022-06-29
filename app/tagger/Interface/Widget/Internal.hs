{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget.Internal (
  TaggerWidget,
  descriptorTreeWidget,
) where

import Control.Lens
import Data.Config
import Data.Event
import qualified Data.List as L
import Data.Model
import Data.Text (Text)
import Database.Tagger.Type
import Interface.Theme
import Monomer

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

descriptorTreeWidget :: TaggerModel -> TaggerWidget
descriptorTreeWidget m =
  vstack_
    []
    [ descriptorTreeWidgetMainPane
    , nodeVisible
        descriptorTreeWidgetAltPane
        (VisibilityLabel "manage" == (m ^. visibilityModel . descriptorTreeVis))
    ]
 where
  descriptorTreeWidgetMainPane =
    vstack_
      []
      [ hstack_
          []
          [ vstack_
              []
              [ descriptorTreeRefreshBothButton
              , descriptorTreeRequestParentButton
              , descriptorTreeFixedRequestButton $
                  m ^. conf . descriptorTreeConf . treeRootRequest
              ]
          , hsplit_
              [splitIgnoreChildResize True]
              ( descriptorTreeFocusedNodeWidget m
              , descriptorTreeUnrelatedWidget m
              )
          ]
      , separatorLine
      , descriptorTreeToggleVisButton
      ]
  descriptorTreeWidgetAltPane =
    withStyleBasic [border 1 black] $
      vstack_
        []
        [ label "sus"
        ]

descriptorTreeFocusedNodeWidget :: TaggerModel -> TaggerWidget
descriptorTreeFocusedNodeWidget m =
  createRelationDropTarget descriptorTreeFocusedNodeWidgetBody
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
    let descriptorInfos = {-L.nub?-} m ^. descriptorTreeModel . focusedTree
        metaDescriptors =
          L.sortOn _descriptorwithInfoDescriptor
            . filter _descriptorwithinfoDescriptorIsMeta
            $ descriptorInfos
        infraDescriptors =
          L.sortOn _descriptorwithInfoDescriptor
            . filter (not . _descriptorwithinfoDescriptorIsMeta)
            $ descriptorInfos
     in vscroll_ [wheelRate 50] . vstack_ [] $
          descriptorWithInfoLabel
            <$> (metaDescriptors ++ infraDescriptors)

  nodeHeader :: TaggerWidget
  nodeHeader = label . descriptor $ m ^. descriptorTreeModel . focusedNode

  createRelationDropTarget :: TaggerWidget -> TaggerWidget
  createRelationDropTarget =
    dropTarget_
      (DoDescriptorTreeEvent . CreateRelation (m ^. descriptorTreeModel . focusedNode))
      [dropTargetStyle [border 3 yuiBlue]]

descriptorTreeUnrelatedWidget :: TaggerModel -> TaggerWidget
descriptorTreeUnrelatedWidget m =
  createUnrelationDropTargetWidget
    descriptorTreeUnrelatedWidgetBody
 where
  descriptorTreeUnrelatedWidgetBody :: TaggerWidget
  descriptorTreeUnrelatedWidgetBody =
    vstack_
      []
      [ label "Unrelated"
      , separatorLine
      , unrelatedTreeLeafWidget
      ]

  unrelatedTreeLeafWidget :: TaggerWidget
  unrelatedTreeLeafWidget =
    let unrelatedDescriptors = m ^. descriptorTreeModel . unrelated
        meta =
          L.sortOn _descriptorwithInfoDescriptor
            . filter _descriptorwithinfoDescriptorIsMeta
            $ unrelatedDescriptors
        infra =
          L.sortOn _descriptorwithInfoDescriptor
            . filter (not . _descriptorwithinfoDescriptorIsMeta)
            $ unrelatedDescriptors
     in vscroll_ [wheelRate 50] . vstack_ [] $ descriptorWithInfoLabel <$> (meta ++ infra)

  createUnrelationDropTargetWidget :: TaggerWidget -> TaggerWidget
  createUnrelationDropTargetWidget =
    dropTarget_
      (DoDescriptorTreeEvent . CreateRelation (m ^. descriptorTreeModel . unrelatedNode))
      [dropTargetStyle [border 3 yuiBlue]]

descriptorWithInfoLabel :: DescriptorWithInfo -> TaggerWidget
descriptorWithInfoLabel (DescriptorWithInfo d@(Descriptor _ dName) isMeta) =
  draggable d
    . withStyleHover [border 1 yuiOrange, bgColor yuiLightPeach]
    . withStyleBasic [textColor (if isMeta then yuiBlue else black), textLeft]
    $ button dName (DoDescriptorTreeEvent (RequestFocusedNode dName))

descriptorTreeToggleVisButton :: TaggerWidget
descriptorTreeToggleVisButton =
  styledButton
    "Manage Descriptors"
    (DoDescriptorTreeEvent (ToggleDescriptorTreeVisibility "manage"))

descriptorTreeFixedRequestButton :: Text -> TaggerWidget
descriptorTreeFixedRequestButton t =
  styledButton "Top" (DoDescriptorTreeEvent . RequestFocusedNode $ t)

descriptorTreeRequestParentButton :: TaggerWidget
descriptorTreeRequestParentButton =
  styledButton "Up" (DoDescriptorTreeEvent RequestFocusedNodeParent)

descriptorTreeRefreshBothButton :: TaggerWidget
descriptorTreeRefreshBothButton =
  styledButton
    "Refresh"
    (DoDescriptorTreeEvent RefreshBothDescriptorTrees)

styledButton :: Text -> TaggerEvent -> TaggerWidget
styledButton t e =
  withStyleHover [bgColor yuiYellow, border 1 yuiOrange]
    . withStyleBasic [bgColor yuiLightPeach, border 0 yuiPeach]
    $ button t e

withStyleBasic ::
  [StyleState] ->
  WidgetNode TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent
withStyleBasic = flip styleBasic

withStyleHover ::
  [StyleState] ->
  WidgetNode TaggerModel TaggerEvent ->
  WidgetNode TaggerModel TaggerEvent
withStyleHover = flip styleHover