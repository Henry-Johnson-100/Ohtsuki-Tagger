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

styledButton :: Text -> TaggerEvent -> TaggerWidget
styledButton t e =
  withStyleHover [bgColor yuiYellow, border 1 yuiOrange]
    . withStyleBasic [bgColor yuiPeach, border 0 yuiPeach]
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
  vstack_
    []
    [ nodeHeader
    , separatorLine
    , focusedTreeLeafWidget
    ]
 where
  nodeHeader :: TaggerWidget
  nodeHeader = label . descriptor $ m ^. descriptorTreeModel . focusedNode

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
   where
    descriptorWithInfoLabel :: DescriptorWithInfo -> TaggerWidget
    descriptorWithInfoLabel (DescriptorWithInfo d@(Descriptor _ dName) isMeta) =
      draggable d
        . withStyleHover [bgColor yuiYellow, border 1 yuiOrange]
        . withStyleBasic [textColor (if isMeta then yuiBlue else black)]
        $ label dName

descriptorTreeUnrelatedWidget :: TaggerModel -> TaggerWidget
descriptorTreeUnrelatedWidget _ = label "descriptorTreeUnrelatedWidget"

descriptorTreeRefreshBothButton :: TaggerWidget
descriptorTreeRefreshBothButton =
  styledButton
    "Refresh"
    (DoDescriptorTreeEvent RefreshBothDescriptorTrees)