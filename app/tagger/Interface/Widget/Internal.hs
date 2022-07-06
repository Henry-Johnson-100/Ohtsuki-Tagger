{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Interface.Widget.Internal (
  TaggerWidget,
  fileSelectionWidget,
  fileSelectionOperationWidget,
  focusedFileWidget,
  descriptorTreeWidget,
) where

import Control.Lens
import Data.Config
import Data.Event
import qualified Data.HashSet as HS
import qualified Data.HierarchyMap as HM
import qualified Data.List as L
import Data.Model
import Data.Model.Shared
import qualified Data.OccurrenceHashMap as OHM
import qualified Data.Ord as O
import qualified Data.Sequence as Seq
import Data.Tagger
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Type
import Interface.Theme
import Monomer

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

{-
 _____ ___ _     _____
|  ___|_ _| |   | ____|
| |_   | || |   |  _|
|  _|  | || |___| |___
|_|   |___|_____|_____|

 ____  _____ _     _____ ____ _____ ___ ___  _   _
/ ___|| ____| |   | ____/ ___|_   _|_ _/ _ \| \ | |
\___ \|  _| | |   |  _|| |     | |  | | | | |  \| |
 ___) | |___| |___| |__| |___  | |  | | |_| | |\  |
|____/|_____|_____|_____\____| |_| |___\___/|_| \_|

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|

-}

manageFileSelectionPane :: Text
manageFileSelectionPane = "manage-file-selection"

editFileMode :: Text
editFileMode = "edit-file"

fileSelectionWidget :: TaggerModel -> TaggerWidget
fileSelectionWidget m =
  vstack_
    []
    [ zstack_
        []
        [ withNodeVisible (not selectionIsVisible) $ tagListWidget m
        , withNodeVisible selectionIsVisible $ fileSelectionFileList m
        ]
    , withNodeVisible (not . Seq.null $ m ^. fileSelectionModel . selection) $
        hstack_
          []
          [clearSelectionButton, toggleViewSelectionButton]
    ]
 where
  selectionIsVisible =
    (m ^. fileSelectionModel . fileSelectionVis) `hasVis` VisibilityAlt
  toggleViewSelectionButton =
    styledButton
      (if selectionIsVisible then "Tags" else "Selection")
      (DoFileSelectionEvent ToggleSelectionView)

fileSelectionOperationWidget :: TaggerModel -> TaggerWidget
fileSelectionOperationWidget _ = queryWidget
 where
  queryWidget =
    keystroke_
      [("Enter", DoFileSelectionEvent Query)]
      []
      . box_ [alignTop, alignCenter]
      $ hstack_ [] [runQueryButton, queryTextField, setOpDropdown]
   where
    runQueryButton = styledButton "Search" (DoFileSelectionEvent Query)
    queryTextFieldKey = "queryTextField"
    queryTextField =
      dropTarget_
        (DoFileSelectionEvent . AppendQueryText . filePath)
        [dropTargetStyle [border 1 yuiOrange]]
        . dropTarget_
          (DoFileSelectionEvent . AppendQueryText . descriptor)
          [dropTargetStyle [border 1 yuiBlue]]
        . withNodeKey queryTextFieldKey
        $ textField_ (fileSelectionModel . queryText) []
    setOpDropdown :: TaggerWidget
    setOpDropdown =
      dropdown_
        (fileSelectionModel . setOp)
        [Union, Intersect, Difference]
        (label . T.pack . show)
        (label . T.pack . show)
        []

fileSelectionFileList :: TaggerModel -> TaggerWidget
fileSelectionFileList m =
  vstack_
    []
    [ vscroll_ [wheelRate 50]
        . vstack_ []
        $ fmap fileSelectionLeaf (m ^. fileSelectionModel . selection)
    , withNodeVisible (not . Seq.null $ m ^. fileSelectionModel . selection) $
        fileSelectionManagePane m
    ]
 where
  fileSelectionLeaf :: File -> TaggerWidget
  fileSelectionLeaf f@(File _ fp) =
    draggable f $
      zstack_
        []
        [ withNodeVisible
            ( not isEditMode
            )
            . withStyleBasic [textLeft]
            $ label_ fp []
        , withNodeVisible isEditMode $ label "edit mode :P"
        ]
   where
    isEditMode =
      (m ^. fileSelectionModel . fileSelectionVis)
        `hasVis` VisibilityLabel editFileMode

tagListWidget :: TaggerModel -> TaggerWidget
tagListWidget m =
  vscroll_ [wheelRate 50] $
    vstack_
      []
      [ tagListHeader
      , separatorLine
      , vstack_ [] (tagListLeaf <$> sortedOccurrenceMapList)
      ]
 where
  tagListHeader =
    hstack_
      []
      [ tagListOrderCritCycleButton
      , tagListOrderDirCycleButton
      , spacer
      , label $
          "In Selection: ("
            <> ( T.pack . show
                  . Seq.length
                  $ m ^. fileSelectionModel . selection
               )
            <> ")"
      ]
  sortedOccurrenceMapList =
    let (OrderBy ordCrit ordDir) = m ^. fileSelectionModel . tagOrdering
        !occurrenceMapList = OHM.toList $ m ^. fileSelectionModel . tagOccurrences
     in case (ordCrit, ordDir) of
          (Alphabetic, Asc) -> L.sortOn (descriptor . fst) occurrenceMapList
          (Alphabetic, Desc) -> L.sortOn (O.Down . descriptor . fst) occurrenceMapList
          (Numeric, Asc) -> L.sortOn snd occurrenceMapList
          (Numeric, Desc) -> L.sortOn (O.Down . snd) occurrenceMapList
  tagListOrderCritCycleButton =
    let (OrderBy ordCrit _) = m ^. fileSelectionModel . tagOrdering
        btnText =
          case ordCrit of
            Alphabetic -> "ABC"
            Numeric -> "123"
     in styledButton btnText (DoFileSelectionEvent CycleTagOrderCriteria)
  tagListOrderDirCycleButton =
    let (OrderBy _ ordDir) = m ^. fileSelectionModel . tagOrdering
     in styledButton
          (T.pack . show $ ordDir)
          (DoFileSelectionEvent CycleTagOrderDirection)
  tagListLeaf (d, n) =
    hgrid_
      []
      [ draggable d . label . descriptor $ d
      , withStyleBasic
          [paddingL 1.5, paddingR 1.5]
          separatorLine
      , label . T.pack . show $ n
      ]

fileSelectionManagePane :: TaggerModel -> TaggerWidget
fileSelectionManagePane m =
  vstack_
    []
    [ styledButton
        "Manage Selection"
        (DoFileSelectionEvent (TogglePaneVisibility manageFileSelectionPane))
    , separatorLine
    , withNodeVisible
        ( (m ^. fileSelectionModel . fileSelectionVis)
            `hasVis` VisibilityLabel manageFileSelectionPane
        )
        $ hstack_ [] [refreshFileSelectionButton, toggleFileEditMode]
    ]

clearSelectionButton :: TaggerWidget
clearSelectionButton = styledButton "Clear" (DoFileSelectionEvent ClearSelection)

refreshFileSelectionButton :: TaggerWidget
refreshFileSelectionButton =
  styledButton
    "Refresh"
    (DoFileSelectionEvent RefreshFileSelection)

toggleFileEditMode :: TaggerWidget
toggleFileEditMode =
  styledButton
    "Edit"
    (DoFileSelectionEvent (TogglePaneVisibility editFileMode))

{-
 _____ ___   ____ _   _ ____  _____ ____
|  ___/ _ \ / ___| | | / ___|| ____|  _ \
| |_ | | | | |   | | | \___ \|  _| | | | |
|  _|| |_| | |___| |_| |___) | |___| |_| |
|_|   \___/ \____|\___/|____/|_____|____/

 _____ ___ _     _____
|  ___|_ _| |   | ____|
| |_   | || |   |  _|
|  _|  | || |___| |___
|_|   |___|_____|_____|

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|

-}

focusedFileWidget :: TaggerModel -> TaggerWidget
focusedFileWidget m =
  box_ []
    . withStyleBasic [minHeight 300]
    $ hsplit_
      [splitIgnoreChildResize True, splitHandleSize 10]
      (focusedFileMainPane, detailPane m)
 where
  focusedFileMainPane =
    dropTarget_
      (DoFocusedFileEvent . PutFile)
      [dropTargetStyle [border 3 yuiOrange]]
      . dropTarget_
        (\(Descriptor dk _) -> DoFocusedFileEvent (TagFile dk Nothing))
        [dropTargetStyle [border 3 yuiBlue]]
      . dropTarget_
        (DoFocusedFileEvent . UnSubTag . concreteTagId)
        [dropTargetStyle [border 1 yuiRed]]
      . withStyleBasic []
      $ ( case m ^. focusedFileModel . renderability of
            RenderAsImage -> imagePreviewRender
            _ -> imagePreviewRender
        )
        (filePath . concreteTaggedFile $ (m ^. focusedFileModel . focusedFile))

imagePreviewRender :: Text -> TaggerWidget
imagePreviewRender fp = image_ fp [fitEither, alignCenter]

detailPane :: TaggerModel -> TaggerWidget
detailPane m@((^. focusedFileModel . focusedFile) -> (ConcreteTaggedFile _ hm)) =
  hstack_
    []
    [ separatorLine
    , detailPaneTagsWidget
    ]
 where
  detailPaneTagsWidget =
    let metaMembers =
          filter (\x -> HM.metaMember x hm && not (HM.infraMember x hm))
            . HM.keys
            $ hm
        topNullMembers =
          filter
            ( \x ->
                not (HM.metaMember x hm)
                  && not (HM.infraMember x hm)
            )
            . HM.keys
            $ hm
     in vscroll_ [wheelRate 50] . withStyleBasic [] $
          vstack_
            []
            [ label (filePath . concreteTaggedFile $m ^. focusedFileModel . focusedFile)
            , separatorLine
            , metaLeaves metaMembers
            , nullMemberLeaves topNullMembers
            , spacer
            , hstack_ [] [deleteTagZone]
            ]
   where
    nullMemberLeaves topNullMembers =
      withStyleBasic [borderB 1 black]
        . vstack_ []
        $ ( \ct@(ConcreteTag tk (Descriptor _ dp) _) ->
              subTagDropTarget tk
                . box_ [alignLeft, alignTop]
                . draggable ct
                $ label dp
          )
          <$> topNullMembers
    metaLeaves :: [ConcreteTag] -> TaggerWidget
    metaLeaves metaMembers = vstack_ [] (flip metaLeaf hm <$> metaMembers)
     where
      metaLeaf l@(ConcreteTag tk (Descriptor _ dp) _) hmap =
        let subtags = HS.toList $ HM.find l hmap
         in if null subtags
              then
                subTagDropTarget tk . box_ [alignLeft, alignTop]
                  . draggable l
                  $ label dp
              else
                withStyleBasic [border 1 black]
                  . hstack_ []
                  $ [ subTagDropTarget tk
                        . box_ [alignLeft, alignTop]
                        . draggable l
                        $ label dp
                    , withStyleBasic [paddingR 2.5, paddingL 2.5] separatorLine
                    , hscroll_ [wheelRate 50] $ vstack (flip metaLeaf hmap <$> subtags)
                    ]
    deleteTagZone :: TaggerWidget
    deleteTagZone =
      dropTarget_
        (DoFocusedFileEvent . DeleteTag . concreteTagId)
        [dropTargetStyle [border 1 yuiRed]]
        . flip styleHoverSet []
        . withStyleBasic [bgColor yuiLightPeach, border 1 yuiPeach]
        $ buttonD_ "Delete" []
    subTagDropTarget tk =
      dropTarget_
        (\(Descriptor dk _) -> DoFocusedFileEvent (TagFile dk (Just tk)))
        [dropTargetStyle [border 1 yuiBlue]]
        . dropTarget_
          ( \ct ->
              DoFocusedFileEvent
                (MoveTag ct (Just tk))
          )
          [dropTargetStyle [border 1 yuiRed]]

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

editDescriptorVis :: Text
editDescriptorVis = "edit-descriptor"

manageDescriptorPaneVis :: Text
manageDescriptorPaneVis = "manage"

descriptorTreeWidget :: TaggerModel -> TaggerWidget
descriptorTreeWidget m =
  withNodeKey "descriptorTree" $
    keystroke_
      [("Ctrl-m", DoDescriptorTreeEvent (ToggleDescriptorTreeVisibility "manage"))]
      [ignoreChildrenEvts]
      $ vstack_
        []
        [ mainPane
        , withNodeVisible
            ( (m ^. descriptorTreeModel . descriptorTreeVis)
                `hasVis` VisibilityLabel manageDescriptorPaneVis
            )
            altPane
        ]
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
        [ insertDescriptorWidget
        , spacer
        , styledButton
            "Edit"
            ( DoDescriptorTreeEvent
                (ToggleDescriptorTreeVisibility editDescriptorVis)
            )
        , spacer
        ]

descriptorTreeFocusedNodeWidget :: TaggerModel -> TaggerWidget
descriptorTreeFocusedNodeWidget m =
  withStyleBasic [borderR 1 black]
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
  nodeHeader = label . descriptor $ m ^. descriptorTreeModel . focusedNode

  createRelationDropTarget :: TaggerWidget -> TaggerWidget
  createRelationDropTarget =
    dropTarget_
      (DoDescriptorTreeEvent . CreateRelation (m ^. descriptorTreeModel . focusedNode))
      [dropTargetStyle [border 3 yuiBlue]]

descriptorTreeUnrelatedWidget :: TaggerModel -> TaggerWidget
descriptorTreeUnrelatedWidget m =
  withStyleBasic [borderL 1 black]
    . createUnrelationDropTargetWidget
      $descriptorTreeUnrelatedWidgetBody
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

insertDescriptorWidget :: TaggerWidget
insertDescriptorWidget =
  keystroke_ [("Enter", DoDescriptorTreeEvent InsertDescriptor)] [] . hstack_ [] $
    [insertButton, textField_ (descriptorTreeModel . newDescriptorText) []]
 where
  insertButton = styledButton "Insert" (DoDescriptorTreeEvent InsertDescriptor)

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
            . withStyleHover [border 1 yuiOrange, bgColor yuiLightPeach]
            . withStyleBasic
              [ textColor (if di ^. descriptorIsMeta then yuiBlue else black)
              , textLeft
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
            $ textField_
              ( descriptorTreeModel
                  . descriptorInfoMap
                  . descriptorInfoAt (fromIntegral dk)
                  . renameText
              )
              []
        , box_ [alignLeft]
            . withStyleHover [bgColor yuiRed, textColor white]
            . withStyleBasic [textColor yuiRed]
            $ button "Delete" (DoDescriptorTreeEvent (DeleteDescriptor d))
        ]

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
    . withStyleBasic [bgColor yuiLightPeach, border 1 yuiPeach]
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

withNodeVisible :: Bool -> TaggerWidget -> TaggerWidget
withNodeVisible = flip nodeVisible

withNodeKey :: Text -> TaggerWidget -> TaggerWidget
withNodeKey = flip nodeKey

descriptorIsMetaInInfoMap :: TaggerModel -> Descriptor -> Bool
descriptorIsMetaInInfoMap
  ((^. descriptorTreeModel . descriptorInfoMap) -> m)
  (Descriptor (fromIntegral -> dk) _) = m ^. descriptorInfoAt dk . descriptorIsMeta