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
  fileSelectionScrollWidgetNodeKey,
  hidePossibleUIVis,
  fileSelectionWidget,
  queryTextFieldKey,
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
import qualified Data.OccurrenceHashMap as OHM
import qualified Data.Ord as O
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger.Type
import Interface.Handler.WidgetQueryRequest
import Interface.Theme
import Monomer
import Monomer.Graphics.Lens
import Monomer.Lens (fixed)
import Util (both)

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

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

fileSelectionScrollWidgetNodeKey :: Text
fileSelectionScrollWidgetNodeKey = "file-selection-scroll-widget"

queryTextFieldKey :: Text
queryTextFieldKey = "queryTextField"

editFileMode :: Text
editFileMode = "edit-file"

fileSelectionWidget :: TaggerModel -> TaggerWidget
fileSelectionWidget m =
  vstack_
    []
    [ fileSelectionWidgetHeader
    , zstack_
        []
        [ withNodeVisible (not selectionIsVisible) $ tagListWidget m
        , withNodeVisible selectionIsVisible $ fileSelectionFileList m
        ]
    ]
 where
  selectionIsVisible =
    (m ^. fileSelectionModel . fileSelectionVis) `hasVis` VisibilityAlt
  fileSelectionWidgetHeader =
    hstack_
      []
      [ selectionSizeLabel m
      , clearSelectionButton
      ]

fileSelectionFileList :: TaggerModel -> TaggerWidget
fileSelectionFileList m =
  vstack_
    []
    [ fileSelectionHeader
    , separatorLine
    , fileListBody
    ]
 where
  fileListBody =
    box_
      [ alignTop
      , alignCenter
      , fileListMergeRequirement
      ]
      . withNodeKey fileSelectionScrollWidgetNodeKey
      . vscroll_
        [ wheelRate 50
        ]
      . vstack_ []
      . flip (|>) (hstack [toggleFileEditMode, addFilesWidget])
      $ ( fmap fileSelectionLeaf renderedChunks
            Seq.>< Seq.fromList fileListPaginationWidgets
        )
   where
    fileListPaginationWidgets =
      [ box_ [alignBottom, alignCenter] $
          hstack_
            []
            [ styledButton_
                [resizeFactor (-1)]
                "<-"
                ( DoFileSelectionEvent
                    . DoFileSelectionWidgetEvent
                    $ CyclePrevChunk
                )
            , label_
                ( ( T.pack . show . succ $
                      m ^. fileSelectionModel . currentChunk
                  )
                    <> "/"
                    <> ( T.pack
                          . show
                          . Seq.length
                          $ m ^. fileSelectionModel . chunkSequence
                       )
                )
                [resizeFactor (-1)]
            , styledButton_
                [resizeFactor (-1)]
                "->"
                ( DoFileSelectionEvent
                    . DoFileSelectionWidgetEvent
                    $ CycleNextChunk
                )
            ]
      ]
    fileListMergeRequirement =
      mergeRequired
        ( \_ m1 m2 ->
            let neq l = m1 ^. fileSelectionModel . l /= m2 ^. fileSelectionModel . l
             in or
                  [ neq selection
                  , neq fileSelectionInfoMap
                  , neq currentChunk
                  , neq chunkSequence
                  , neq chunkSize
                  , neq fileSelectionVis
                  ]
        )
  renderedChunks =
    getSelectionChunk m
  fileSelectionHeader :: TaggerWidget
  fileSelectionHeader =
    hstack_
      []
      [ toggleViewSelectionButton
      , shuffleSelectionButton
      , refreshFileSelectionButton
      , fileSelectionChunkSizeNumField
      , -- , toggleFileEditMode
        shellCommandWidget m
      ]
  fileSelectionLeaf :: File -> TaggerWidget
  fileSelectionLeaf f@(File fk fp) =
    zstack_
      []
      [ withNodeVisible
          ( not isEditMode
          )
          . draggable f
          . withStyleBasic [textLeft]
          $ label_ fp [resizeFactor (-1)]
      , withNodeVisible isEditMode editModeWidget
      ]
   where
    editModeWidget =
      hstack_
        []
        [ removeFromSelectionButton
        , renameFileTextField
        , removeFileFromDatabaseButton
        , deleteFileFromFileSystemButton
        ]
     where
      deleteFileFromFileSystemButton :: TaggerWidget
      deleteFileFromFileSystemButton =
        zstack
          [ withNodeVisible confirmDeleteVis
              . tooltip_
                "Deletes the file from the database and file system."
                [tooltipDelay 500]
              . withStyleHover [textColor white, bgColor yuiRed, border 1 black]
              . withStyleBasic [textColor yuiRed, bgColor yuiLightPeach, border 1 black]
              $ styledButton_
                [resizeFactor (-1)]
                "Confirm"
                (DoFileSelectionEvent . DeleteFileFromFileSystem $ fk)
          , withNodeVisible
              (not confirmDeleteVis)
              . tooltip_
                "Deletes the file from the database and file system."
                [tooltipDelay 500]
              . withStyleHover [textColor white, bgColor yuiRed, border 1 black]
              . withStyleBasic [textColor yuiRed, bgColor yuiLightPeach, border 1 black]
              $ toggleButton_
                "Delete"
                ( fileSelectionModel
                    . fileSelectionInfoMap
                    . fileInfoAt (fromIntegral fk)
                    . deleteFileIsVis
                )
                [resizeFactor (-1)]
          ]
       where
        confirmDeleteVis =
          m
            ^. fileSelectionModel
              . fileSelectionInfoMap
              . fileInfoAt (fromIntegral fk)
              . deleteFileIsVis
      removeFileFromDatabaseButton :: TaggerWidget
      removeFileFromDatabaseButton =
        withStyleBasic [bgColor yuiLightPeach]
          . tooltip_ "Removes the file from the database only." [tooltipDelay 1500]
          . withStyleHover [bgColor yuiRed]
          $ styledButton_
            [resizeFactor (-1)]
            "Remove"
            (DoFileSelectionEvent . RemoveFileFromDatabase $ fk)
      renameFileTextField :: TaggerWidget
      renameFileTextField =
        keystroke_
          [("Enter", DoFileSelectionEvent . RenameFile $ fk)]
          [ignoreChildrenEvts]
          $ textField_
            ( fileSelectionModel
                . fileSelectionInfoMap
                . fileInfoAt (fromIntegral fk)
                . fileInfoRenameText
            )
            []
      removeFromSelectionButton =
        styledButton_
          [resizeFactor (-1)]
          "-"
          (DoFileSelectionEvent (RemoveFileFromSelection fk))
    isEditMode =
      (m ^. fileSelectionModel . fileSelectionVis)
        `hasVis` VisibilityLabel editFileMode

tagListWidget :: TaggerModel -> TaggerWidget
tagListWidget m =
  vstack_
    []
    [ tagListHeader
    , separatorLine
    , vscroll_ [wheelRate 50]
        . box_
          [ alignTop
          , alignCenter
          , mergeRequired
              ( \_ m1 m2 ->
                  let neq l = m1 ^. fileSelectionModel . l /= m2 ^. fileSelectionModel . l
                   in or [neq tagOccurrences, neq tagOrdering]
              )
          ]
        $ vstack_ [] (tagListLeaf <$> sortedOccurrenceMapList)
    ]
 where
  tagListHeader =
    hstack_
      []
      [ tagListOrderCritCycleButton
      , tagListOrderDirCycleButton
      , toggleViewSelectionButton
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
     in styledButton_
          [resizeFactor (-1)]
          btnText
          (DoFileSelectionEvent CycleTagOrderCriteria)
  tagListOrderDirCycleButton =
    let (OrderBy _ ordDir) = m ^. fileSelectionModel . tagOrdering
     in styledButton_
          [resizeFactor (-1)]
          (T.pack . show $ ordDir)
          (DoFileSelectionEvent CycleTagOrderDirection)
  tagListLeaf (d, n) =
    hgrid_
      [childSpacing_ 0]
      [ draggable d . withStyleBasic [textRight, paddingR 1.5]
          . flip label_ []
          . descriptor
          $ d
      , withStyleBasic [paddingL 1.5, borderL 1 black] . label . T.pack . show $ n
      ]

clearSelectionButton :: TaggerWidget
clearSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "Clear"
    (DoFileSelectionEvent ClearQuerySequence)

toggleViewSelectionButton :: TaggerWidget
toggleViewSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "View"
    (DoFileSelectionEvent ToggleSelectionView)

shellCommandWidget :: TaggerModel -> TaggerWidget
shellCommandWidget ((^. isMassOpMode) -> isMassOpModeIsTrue) =
  box_ [sizeReqUpdater (both (& fixed .~ 0))] $
    hstack
      [ toggleButton_ "MassOp" isMassOpMode []
      , keystroke_
          [
            ( "Enter"
            , if isMassOpModeIsTrue
                then DoFileSelectionEvent RunSelectionShellCommand
                else DoFocusedFileEvent RunFocusedFileShellCommand
            )
          ]
          [ignoreChildrenEvts]
          . withStyleBasic [minWidth 80]
          $ textField_ shellText []
      ]

fileSelectionChunkSizeNumField :: TaggerWidget
fileSelectionChunkSizeNumField =
  withStyleBasic [maxWidth 80] $
    numericField_ (fileSelectionModel . chunkSize) [minValue 0]

addFilesWidget :: TaggerWidget
addFilesWidget =
  keystroke
    [ ("Enter", DoFileSelectionEvent AddFiles)
    , ("Up", DoFileSelectionEvent NextAddFileHist)
    , ("Down", DoFileSelectionEvent PrevAddFileHist)
    ]
    $ hstack_
      []
      [ styledButton_ [resizeFactor (-1)] "Add" (DoFileSelectionEvent AddFiles)
      , textField_
          (fileSelectionModel . addFileText)
          [ onChange
              ( \t ->
                  if T.null t
                    then DoFileSelectionEvent ResetAddFileHistIndex
                    else IOEvent ()
              )
          ]
      ]

selectionSizeLabel :: TaggerModel -> TaggerWidget
selectionSizeLabel m =
  flip label_ [resizeFactor (-1)] $
    "In Selection: ("
      <> ( T.pack . show
            . Seq.length
            $ m ^. fileSelectionModel . selection
         )
      <> ")"

refreshFileSelectionButton :: TaggerWidget
refreshFileSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "Refresh"
    (DoFileSelectionEvent RefreshFileSelection)

toggleFileEditMode :: TaggerWidget
toggleFileEditMode =
  styledButton_
    [resizeFactor (-1)]
    "Edit"
    (DoFileSelectionEvent (TogglePaneVisibility editFileMode))

shuffleSelectionButton :: TaggerWidget
shuffleSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "Shuffle"
    (DoFileSelectionEvent ShuffleSelection)

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
            , hstack [zstackPrevImage]
            ]
      , queryWidget m
      ]
   where
    zstackNextImage =
      withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
        styledButton_ [resizeFactor (-1)] "↑" (DoFileSelectionEvent CycleNextFile)
    zstackPrevImage =
      withStyleBasic [bgColor $ yuiLightPeach & a .~ mainPaneFloatingOpacity] $
        styledButton_ [resizeFactor (-1)] "↓" (DoFileSelectionEvent CyclePrevFile)
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

queryWidget :: TaggerModel -> TaggerWidget
queryWidget m =
  box_ [alignLeft, ignoreEmptyArea]
    . withStyleBasic [maxWidth 450]
    $ vstack_
      []
      [ widgetQueryRequestWidget
      , hstack_
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
          , withNodeVisible
              ( (m ^. focusedFileModel . focusedFileVis)
                  `hasVis` VisibilityLabel zstackQueryWidgetVis
              )
              queryTextField
          ]
      ]
 where
  widgetQueryRequestWidget =
    vstack_
      []
      ( widgetQueryNode
          <$> widgetQueryRequest
            (m ^. fileSelectionModel . fileSelectionQueryWidgetRequest)
      )
   where
    widgetQueryNode wsb@(WidgetSentenceBranchComp t c _) =
      hstack
        [ styledButton_
            [resizeFactor (-1)]
            "-"
            (DoFileSelectionEvent . DeleteQueryNode $ wsb)
        , dropTarget_
            (DoFileSelectionEvent . flip MoveQueryNodeBefore wsb)
            [dropTargetStyle [borderT 1 yuiOrange]]
            . draggable wsb
            $ label_
              ( t
                  <> " : "
                  <> (T.pack . show $ c)
              )
              [resizeFactor (-1)]
        ]
    widgetQueryNode _ = label "_ match in widgetQueryNode"

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
              , withStyleBasic [maxHeight 400] $ fileSelectionWidget m
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

queryTextField :: TaggerWidget
queryTextField =
  keystroke_
    [ ("Enter", DoFileSelectionEvent CreateNewWidgetQueryNode)
    , ("Shift-Enter", DoFileSelectionEvent RunQuerySequence)
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
    . withStyleBasic [bgColor (yuiLightPeach & a .~ mainPaneFloatingOpacity)]
    $ textField_
      (fileSelectionModel . queryText)
      [ onChange
          ( \t ->
              if T.null t
                then DoFileSelectionEvent ResetQueryHistIndex
                else IOEvent ()
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

styledButton_ ::
  [ButtonCfg TaggerModel TaggerEvent] ->
  Text ->
  TaggerEvent ->
  TaggerWidget
styledButton_ opts t e =
  withStyleHover [bgColor yuiYellow, border 1 yuiOrange]
    . withStyleBasic [bgColor yuiLightPeach, border 1 yuiPeach]
    $ button_ t e opts

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