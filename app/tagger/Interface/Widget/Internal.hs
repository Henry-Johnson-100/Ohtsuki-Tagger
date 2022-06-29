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
import qualified Data.Text as T
import Database.Tagger.Type
import Interface.Theme
import Monomer

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

{-
 ____  _____ ____   ____ ____  ___ ____ _____ ___  ____
|  _ \| ____/ ___| / ___|  _ \|_ _|  _ \_   _/ _ \|  _ \
| | | |  _| \___ \| |   | |_) || || |_) || || | | | |_) |
| |_| | |___ ___) | |___|  _ < | ||  __/ | || |_| |  _ <
|____/|_____|____/ \____|_| \_\___|_|    |_| \___/|_| \_\

 _____ ____  _____ _____
|_   _|  _ \| ____| ____|
  | | | |_) |  _| |  _|
  | | |  _ <| |___| |___
  |_| |_| \_\_____|_____|

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|
-}

descriptorTreeWidget :: TaggerModel -> TaggerWidget
descriptorTreeWidget m =
  vstack_
    []
    [ mainPane
    , nodeVisible
        altPane
        (VisibilityLabel "manage" == (m ^. visibilityModel . descriptorTreeVis))
    ]
 where
  mainPane =
    vstack_
      []
      [ hstack_
          []
          [ mainPaneLeftButtonStack
          , hsplit_
              [splitIgnoreChildResize True]
              ( descriptorTreeFocusedNodeWidget m
              , descriptorTreeUnrelatedWidget m
              )
          ]
      , separatorLine
      , descriptorTreeToggleVisButton
      ]
   where
    mainPaneLeftButtonStack =
      vstack_
        []
        [ descriptorTreeRefreshBothButton
        , descriptorTreeRequestParentButton
        , descriptorTreeFixedRequestButton $
            m ^. conf . descriptorTreeConf . treeRootRequest
        ]
  altPane =
    withStyleBasic [border 1 black] $
      vstack_
        []
        [ updateDescriptorWidget m
        , spacer
        , insertDescriptorWidget
        , spacer
        , box_ [alignMiddle] deleteDescriptorWidget
        , spacer
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

updateDescriptorWidget :: TaggerModel -> TaggerWidget
updateDescriptorWidget m =
  dropTarget_
    (DoDescriptorTreeEvent . PutUpdateDescriptorFrom)
    [dropTargetStyle [border 1 yuiOrange]]
    . keystroke_ [("Enter", DoDescriptorTreeEvent UpdateDescriptor)] [ignoreChildrenEvts]
    $ hstack_
      []
      [ updateDescriptorButton
      , hgrid_
          []
          [ box_ [alignMiddle] updateDescriptorFromLabelWidget
          , updateDescriptorToTextField
          ]
      ]
 where
  updateDescriptorButton :: TaggerWidget
  updateDescriptorButton = styledButton "Rename" (DoDescriptorTreeEvent UpdateDescriptor)

  updateDescriptorFromLabelWidget :: TaggerWidget
  updateDescriptorFromLabelWidget =
    label
      ( maybe
          "No Descriptor"
          (\(Descriptor dk dp) -> (T.pack . show $ dk) <> " : " <> dp)
          (m ^. descriptorTreeModel . updateDescriptorFrom)
      )

  updateDescriptorToTextField :: TaggerWidget
  updateDescriptorToTextField = textField_ (descriptorTreeModel . updateDescriptorTo) []

insertDescriptorWidget :: TaggerWidget
insertDescriptorWidget =
  keystroke_ [("Enter", DoDescriptorTreeEvent InsertDescriptor)] [] . hstack_ [] $
    [insertButton, textField_ (descriptorTreeModel . newDescriptorText) []]
 where
  insertButton = styledButton "Insert" (DoDescriptorTreeEvent InsertDescriptor)

deleteDescriptorWidget :: TaggerWidget
deleteDescriptorWidget =
  withStyleBasic [border 1 yuiRed]
    . dropTarget_
      (DoDescriptorTreeEvent . DeleteDescriptor)
      [dropTargetStyle [bgColor yuiRed, border 1 yuiYellow]]
    $ label "Delete"

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