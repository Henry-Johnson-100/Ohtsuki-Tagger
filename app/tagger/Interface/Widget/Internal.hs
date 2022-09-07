{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Use ||" #-}

module Interface.Widget.Internal (
  TaggerWidget,
  hidePossibleUIVis,
  -- fileSelectionOperationWidget,
  tagTextNodeKey,
  zstackTaggingWidgetVis,
  zstackQueryWidgetVis,
  focusedFileWidget,
  descriptorTreeWidget,
  taggerInfoWidget,
) where

import Control.Lens hiding (both)
import Data.Event
import qualified Data.HashSet as HS
import qualified Data.HierarchyMap as HM
import qualified Data.List as L
import Data.Model
import Data.Model.Shared
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Type
import Interface.Theme
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer
import Monomer.Graphics.Lens
import Monomer.Lens (fixed)
import Util (both)

hidePossibleUIVis :: Text
hidePossibleUIVis = "hide-possible-elements"

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

mainPaneFloatingOpacity :: Double
mainPaneFloatingOpacity = 0.5

tagTextNodeKey :: Text
tagTextNodeKey = "tag-text-field"

zstackTaggingWidgetVis :: Text
zstackTaggingWidgetVis = "show-tag-field"

zstackQueryWidgetVis :: Text
zstackQueryWidgetVis = "show-query-field"

focusedFileWidget :: TaggerModel -> TaggerWidget
focusedFileWidget m =
  box_ []
    . withStyleBasic [minHeight 300]
    $ hsplit_
      [splitIgnoreChildResize True, splitHandleSize 10]
      ( withStyleBasic [borderR 1 black] focusedFileMainPane
      , withNodeVisible
          (not $ (m ^. visibilityModel) `hasVis` VisibilityLabel hidePossibleUIVis)
          $ detailPane m
      )
 where
  focusedFileMainPane =
    zstack_
      [onlyTopActive_ False]
      [ dropTarget_
          (DoFocusedFileEvent . PutFile)
          [dropTargetStyle [border 3 yuiOrange]]
          . dropTarget_
            (\(Descriptor dk _) -> DoFocusedFileEvent (TagFile dk Nothing))
            [dropTargetStyle [border 3 yuiBlue]]
          . dropTarget_
            (DoFocusedFileEvent . UnSubTag . concreteTagId)
            [dropTargetStyle [border 1 yuiRed]]
          . withStyleBasic []
          . box_
            [ mergeRequired
                ( \_ m1 m2 ->
                    concreteTaggedFile (m1 ^. focusedFileModel . focusedFile)
                      /= concreteTaggedFile (m2 ^. focusedFileModel . focusedFile)
                )
            ]
          $ ( case m ^. focusedFileModel . renderability of
                RenderAsImage -> imagePreviewRender
                _ -> imagePreviewRender
            )
            (filePath . concreteTaggedFile $ (m ^. focusedFileModel . focusedFile))
      , withNodeVisible
          ( not $
              (m ^. visibilityModel) `hasVis` VisibilityLabel hidePossibleUIVis
          )
          . box_
            [alignBottom, alignLeft, ignoreEmptyArea]
          $ vstack
            [ hstack [zstackNextImage, zstackTaggingWidget]
            , hstack [zstackPrevImage, zstackQueryWidget]
            ]
      ]
   where
    zstackNextImage =
      withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
        styledButton_ [resizeFactor (-1)] "↑" (DoFileSelectionEvent CycleNextFile)
    zstackPrevImage =
      withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
        styledButton_ [resizeFactor (-1)] "↓" (DoFileSelectionEvent CyclePrevFile)
    zstackQueryWidget :: TaggerWidget
    zstackQueryWidget =
      box_ [alignLeft, ignoreEmptyArea]
        . withStyleBasic [maxWidth 450]
        $ hstack_
          []
          [ vstack . (: [])
              . withStyleBasic
                [ bgColor $
                    yuiLightPeach
                      & a .~ mainPaneFloatingOpacity
                ]
              $ styledButton_
                [resizeFactor (-1)]
                "Query"
                ( DoFocusedFileEvent
                    (ToggleFocusedFilePaneVisibility zstackQueryWidgetVis)
                )
                -- , withNodeVisible isVisible queryTextField
          ]
     where
    -- isVisible =
    --   (m ^. focusedFileModel . focusedFileVis)
    --     `hasVis` VisibilityLabel zstackQueryWidgetVis
    zstackTaggingWidget :: TaggerWidget
    zstackTaggingWidget =
      box_ [alignLeft, ignoreEmptyArea]
        . withStyleBasic [maxWidth 400]
        $ hstack
          [ vstack . (: [])
              . withStyleBasic
                [ bgColor $
                    yuiLightPeach
                      & a .~ mainPaneFloatingOpacity
                ]
              $ styledButton_
                [resizeFactor (-1)]
                "Tag"
                ( DoFocusedFileEvent
                    (ToggleFocusedFilePaneVisibility zstackTaggingWidgetVis)
                )
          , withNodeVisible
              isVisible
              tagTextField
          ]
     where
      isVisible =
        (m ^. focusedFileModel . focusedFileVis)
          `hasVis` VisibilityLabel zstackTaggingWidgetVis

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
    withStyleBasic
      [paddingR 20]
      $ vstack_
        []
        [ vstack_
            []
            [ filePathWidget
            , separatorLine
            , vscroll_ [wheelRate 50] $
                vstack
                  [ metaLeaves
                      ( L.sortOn (descriptor . concreteTagDescriptor)
                          . filter
                            ( \x ->
                                HM.metaMember x hm
                                  && not (HM.infraMember x hm)
                            )
                          . HM.keys
                          $ hm
                      )
                  , spacer_ [resizeFactor (-1)]
                  , nullMemberLeaves
                      ( L.sortOn (descriptor . concreteTagDescriptor)
                          . filter
                            ( \x ->
                                not (HM.metaMember x hm)
                                  && not (HM.infraMember x hm)
                            )
                          . HM.keys
                          $ hm
                      )
                  , deleteTagZone
                  ]
            ]
        , withStyleBasic [paddingT 20] $
            vstack
              [ separatorLine
              , withStyleBasic [maxHeight 400] $ undefined m
              ]
        ]
   where
    filePathWidget :: TaggerWidget
    filePathWidget =
      hstack_
        []
        [ withNodeVisible
            ( focusedFileDefaultRecordKey
                /= (fileId . concreteTaggedFile $ m ^. focusedFileModel . focusedFile)
            )
            $ styledButton_
              [resizeFactor (-1)]
              "Rename"
              ( DoFocusedFileEvent
                  (ToggleFocusedFilePaneVisibility fileRenameModeVis)
              )
        , zstack_
            []
            [ withNodeVisible (not isFileRenameMode) $
                flip
                  label_
                  [resizeFactor (-1)]
                  (filePath . concreteTaggedFile $m ^. focusedFileModel . focusedFile)
            , withNodeVisible isFileRenameMode
                . keystroke_
                  [
                    ( "Enter"
                    , DoFileSelectionEvent
                        . RenameFile
                        $ ( fileId . concreteTaggedFile $
                              m ^. focusedFileModel . focusedFile
                          )
                    )
                  ]
                  []
                $ textField_
                  ( fileSelectionModel
                      . fileSelectionInfoMap
                      . fileInfoAt
                        ( fromIntegral
                            . fileId
                            . concreteTaggedFile
                            $ m ^. focusedFileModel . focusedFile
                        )
                      . fileInfoRenameText
                  )
                  []
            ]
        ]
     where
      fileRenameModeVis = "file-rename"
      isFileRenameMode =
        (m ^. focusedFileModel . focusedFileVis)
          `hasVis` VisibilityLabel fileRenameModeVis
    nullMemberLeaves members =
      withStyleBasic [borderB 1 black]
        . vstack_ []
        $ ( \ct@(ConcreteTag tk (Descriptor _ dp) _) ->
              subTagDropTarget tk
                . box_ [alignLeft, alignTop]
                . draggable ct
                $ label dp
          )
          <$> members
    metaLeaves :: [ConcreteTag] -> TaggerWidget
    metaLeaves members =
      vstack_ [] . L.intersperse spacer $
        (flip metaLeaf hm <$> members)
     where
      metaLeaf l@(ConcreteTag tk (Descriptor _ dp) _) hmap =
        let subtags =
              L.sortOn (descriptor . concreteTagDescriptor)
                . HS.toList
                $ HM.find l hmap
         in if null subtags
              then
                subTagDropTarget tk . box_ [alignLeft, alignTop]
                  . draggable l
                  $ label dp
              else
                vstack_
                  []
                  [ hstack_
                      []
                      [ subTagDropTarget tk
                          . box_ [alignLeft, alignTop]
                          . draggable l
                          . withStyleBasic [textColor yuiBlue]
                          $ label dp
                      , spacer
                      , label "{"
                      ]
                  , hstack_
                      []
                      [ metaTagLeafSpacer
                      , box_ [alignLeft, alignTop] $
                          vstack
                            ( flip metaLeaf hmap
                                <$> subtags
                            )
                      ]
                  , label "}"
                  ]
       where
        metaTagLeafSpacer = spacer_ [width 20]
    deleteTagZone :: TaggerWidget
    deleteTagZone =
      dropTarget_
        (DoFocusedFileEvent . DeleteTag . concreteTagId)
        [dropTargetStyle [border 1 yuiRed]]
        . flip styleHoverSet []
        . withStyleBasic [bgColor yuiLightPeach, border 1 yuiPeach]
        $ buttonD_ "Delete" [resizeFactor (-1)]
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

tagTextField :: TaggerWidget
tagTextField =
  keystroke_
    [ ("Enter", DoFocusedFileEvent CommitTagText)
    , ("Up", DoFocusedFileEvent NextTagHist)
    , ("Down", DoFocusedFileEvent PrevTagHist)
    ]
    []
    . dropTarget_
      (DoFocusedFileEvent . AppendTagText . descriptor . concreteTagDescriptor)
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      (DoFocusedFileEvent . AppendTagText . descriptor)
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey tagTextNodeKey
    . withStyleBasic [bgColor (yuiLightPeach & a .~ mainPaneFloatingOpacity)]
    $ textField_
      (focusedFileModel . tagText)
      [ onChange
          ( \t ->
              if T.null t
                then DoFocusedFileEvent ResetTagHistIndex
                else
                  IOEvent
                    ()
          )
      ]

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

descriptorTreeWidget :: TaggerModel -> TaggerWidget
descriptorTreeWidget m =
  withNodeVisible
    ( not $
        (m ^. visibilityModel)
          `hasVis` VisibilityLabel hidePossibleUIVis
    )
    . withNodeKey "descriptorTree"
    $ mainPane
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
      [ flip label_ [resizeFactor (-1)] "Unrelated"
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
            $ button_
              "Delete"
              ( DoDescriptorTreeEvent
                  (DeleteDescriptor d)
              )
              [resizeFactor (-1)]
        ]

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

descriptorTreeFixedRequestButton :: Text -> TaggerWidget
descriptorTreeFixedRequestButton t =
  styledButton_
    [resizeFactor (-1)]
    "Top"
    (DoDescriptorTreeEvent . RequestFocusedNode $ t)

descriptorTreeRequestParentButton :: TaggerWidget
descriptorTreeRequestParentButton =
  styledButton_
    [resizeFactor (-1)]
    "Up"
    (DoDescriptorTreeEvent RequestFocusedNodeParent)

descriptorTreeRefreshBothButton :: TaggerWidget
descriptorTreeRefreshBothButton =
  styledButton_
    [resizeFactor (-1)]
    "Refresh"
    (DoDescriptorTreeEvent RefreshBothDescriptorTrees)

descriptorIsMetaInInfoMap :: TaggerModel -> Descriptor -> Bool
descriptorIsMetaInInfoMap
  ((^. descriptorTreeModel . descriptorInfoMap) -> m)
  (Descriptor (fromIntegral -> dk) _) = m ^. descriptorInfoAt dk . descriptorIsMeta

{-
 _____  _    ____  ____ _____ ____
|_   _|/ \  / ___|/ ___| ____|  _ \
  | | / _ \| |  _| |  _|  _| | |_) |
  | |/ ___ \ |_| | |_| | |___|  _ <
  |_/_/   \_\____|\____|_____|_| \_\

 ___ _   _ _____ ___
|_ _| \ | |  ___/ _ \
 | ||  \| | |_ | | | |
 | || |\  |  _|| |_| |
|___|_| \_|_|   \___/

__        _____ ____   ____ _____ _____
\ \      / /_ _|  _ \ / ___| ____|_   _|
 \ \ /\ / / | || | | | |  _|  _|   | |
  \ V  V /  | || |_| | |_| | |___  | |
   \_/\_/  |___|____/ \____|_____| |_|
-}

taggerInfoWidget :: TaggerModel -> TaggerWidget
taggerInfoWidget m@((^. taggerInfoModel) -> tim) =
  withNodeVisible
    ( not $
        (m ^. visibilityModel)
          `hasVis` VisibilityLabel hidePossibleUIVis
    )
    . box_ [alignMiddle]
    $ vstack $
      withStyleBasic [paddingT 2.5, paddingB 2.5]
        <$> ( [ flip label_ [resizeFactor (-1)] $ tim ^. message
              , flip label_ [resizeFactor (-1)] $ tim ^. versionMessage
              ]
                ++ ( (\(h, t) -> label_ (h <> ": " <> (tim ^. t)) [resizeFactor (-1)])
                      <$> [ ("In Directory", workingDirectory)
                          , ("Version", version)
                          , ("Last Accessed", lastAccessed)
                          ]
                   )
            )