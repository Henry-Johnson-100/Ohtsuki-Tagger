{-# LANGUAGE OverloadedStrings #-}

module Interface.Widget.Internal (
  TaggerWidget,
  descriptorTreeWidget,
) where

import Control.Lens
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
  hstack_
    []
    [ vstack_ [] [descriptorTreeRefreshBothButton]
    , hsplit_
        []
        ( descriptorTreeFocusedNodeWidget m
        , descriptorTreeUnrelatedWidget m
        )
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
      [dropTargetStyle [bgColor yuiYellow, border 1 yuiOrange]]

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
      [dropTargetStyle [bgColor yuiYellow, border 1 yuiBlue]]

descriptorWithInfoLabel :: DescriptorWithInfo -> TaggerWidget
descriptorWithInfoLabel (DescriptorWithInfo d@(Descriptor _ dName) isMeta) =
  draggable d
    . withStyleHover [bgColor yuiYellow, border 1 yuiOrange]
    . withStyleBasic [textColor (if isMeta then yuiBlue else black)]
    $ label dName

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