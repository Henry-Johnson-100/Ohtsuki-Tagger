{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use ||" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.Selection (
  widget,
  fileSelectionScrollWidgetNodeKey,
) where

import Control.Lens ((&), (.~), (^.))
import Data.Event (
  AddFileEvent (AddFilePath, AddFiles, ToggleAddFileVisibility),
  FileSelectionEvent (
    CycleOrderCriteria,
    CycleOrderDirection,
    DeleteFileFromFileSystem,
    DoFileSelectionWidgetEvent,
    IncludeTagListInfraToPattern,
    RefreshFileSelection,
    RemoveFileFromDatabase,
    RemoveFileFromSelection,
    RenameFile,
    RunSelectionShellCommand,
    ShuffleSelection,
    TagSelect,
    TagSelectWholeChunk,
    ToggleSelectionView
  ),
  FileSelectionWidgetEvent (CycleNextChunk, CyclePrevChunk),
  FocusedFileEvent (RunFocusedFileShellCommand),
  TaggerEvent (
    DoAddFileEvent,
    DoFileSelectionEvent,
    DoFocusedFileEvent,
    Mempty,
    NextHistory,
    PrevHistory,
    ToggleVisibilityLabel,
    Unit
  ),
  anonymousEvent,
 )
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Model.Core (TaggerModel, getSelectionChunk)
import Data.Model.Lens (
  HasChunkSequence (chunkSequence),
  HasChunkSize (chunkSize),
  HasCurrentChunk (currentChunk),
  HasDeleteFileIsVis (deleteFileIsVis),
  HasFileInfoRenameText (fileInfoRenameText),
  HasFileSelectionInfoMap (fileSelectionInfoMap),
  HasFileSelectionModel (fileSelectionModel),
  HasFileSelectionVis (fileSelectionVis),
  HasIsMassOpMode (isMassOpMode),
  HasOccurrences (occurrences),
  HasOrdering (ordering),
  HasSelection (selection),
  HasShellText (shellText),
  TaggerLens (TaggerLens),
  addFileModel,
  directoryList,
  fileInfoAt,
  fileSelectionTagListModel,
  inProgress,
  include,
  input,
  isTagSelection,
  tagInputModel,
  taggingSelection,
  visibility,
 )
import Data.Model.Shared.Core (
  OrderBy (OrderBy),
  OrderCriteria (Alphabetic, Numeric),
  OrderDirection (Asc, Desc),
  Visibility (VisibilityAlt, VisibilityLabel, VisibilityMain),
  hasVis,
 )
import Data.Model.Shared.Lens (
  HasHistory (history),
  HasHistoryIndex (historyIndex),
  HasText (text),
 )
import qualified Data.Ord as O
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (Descriptor (descriptor), File (File), descriptorId)
import Interface.Theme (yuiBlack, yuiLightPeach, yuiOrange, yuiRed, yuiWhite)
import Interface.Widget.Internal.Core (
  defaultElementOpacity,
  defaultOpacityModulator,
  modulateOpacity,
  styledButton_,
  styledToggleButton_,
  withNodeKey,
  withNodeVisible,
  withStyleBasic,
  withStyleHover,
 )
import Monomer (
  CmbAlignBottom (alignBottom),
  CmbAlignCenter (alignCenter),
  CmbAlignTop (alignTop),
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbBorderL (borderL),
  CmbChildSpacing (childSpacing_),
  CmbIgnoreChildrenEvts (ignoreChildrenEvts),
  CmbMaxWidth (maxWidth),
  CmbMergeRequired (mergeRequired),
  CmbMinValue (minValue),
  CmbMinWidth (minWidth),
  CmbOnChange (onChange),
  CmbPaddingL (paddingL),
  CmbPaddingR (paddingR),
  CmbResizeFactor (resizeFactor),
  CmbSizeReqUpdater (sizeReqUpdater),
  CmbTextColor (textColor),
  CmbTextLeft (textLeft),
  CmbTextRight (textRight),
  CmbWheelRate (wheelRate),
  EventResponse (..),
  WidgetNode,
  box_,
  draggable,
  hgrid_,
  hstack,
  hstack_,
  keystroke,
  keystroke_,
  label,
  label_,
  numericField_,
  onlyTopActive,
  separatorLine,
  styleIf,
  textFieldV,
  textField_,
  toggleButton_,
  tooltipDelay,
  tooltip_,
  vscroll_,
  vstack_,
  zstack,
  zstack_,
 )
import Monomer.Core.Lens (fixed)
import Util (both)

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

widget :: TaggerModel -> TaggerWidget
widget m =
  vstack_
    []
    [ fileSelectionWidgetHeader
    , zstack_
        []
        [ withNodeVisible (not selectionIsVisible) $ tagListWidget m
        , withNodeVisible selectionIsVisible . vstack_ [] $
            [ fileSelectionFileList m
            , addFilesWidget m
            ]
        ]
    ]
 where
  selectionIsVisible =
    (m ^. fileSelectionModel . fileSelectionVis) `hasVis` VisibilityAlt
  fileSelectionWidgetHeader =
    hstack_
      []
      [ selectionSizeLabel m
      ]

selectionSizeLabel :: TaggerModel -> TaggerWidget
selectionSizeLabel ((^. fileSelectionModel . selection) -> selSeq) =
  withStyleBasic [maxWidth 80]
    . box_ [alignCenter]
    . flip label_ [resizeFactor (-1)]
    $ ( T.pack . show
          . Seq.length
          $ selSeq
      )

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
                  let neq l =
                        m1 ^. fileSelectionTagListModel . l
                          /= m2 ^. fileSelectionTagListModel . l
                   in or [neq occurrences, neq ordering, neq include]
              )
          ]
        $ vstack_
          []
          ( let includeSet = m ^. fileSelectionTagListModel . include
             in if HS.null includeSet
                  then map tagListLeaf sortedOccurrenceMapList
                  else
                    [ tagListLeaf x
                    | x <-
                        sortedOccurrenceMapList
                    , HS.member (descriptorId . fst $ x) includeSet
                    ]
          )
    ]
 where
  tagListHeader =
    hstack_
      []
      [ toggleViewSelectionButton
      , tagListOrderCritCycleButton
      , tagListOrderDirCycleButton
      , tagListFilterTextField
      ]
  sortedOccurrenceMapList =
    let (OrderBy ordCrit ordDir) = m ^. fileSelectionTagListModel . ordering
        occurrenceMapList = HM.toList $ m ^. fileSelectionTagListModel . occurrences
     in case (ordCrit, ordDir) of
          (Alphabetic, Asc) -> L.sortOn (descriptor . fst) occurrenceMapList
          (Alphabetic, Desc) -> L.sortOn (O.Down . descriptor . fst) occurrenceMapList
          (Numeric, Asc) -> L.sortOn snd occurrenceMapList
          (Numeric, Desc) -> L.sortOn (O.Down . snd) occurrenceMapList
  tagListOrderCritCycleButton =
    let (OrderBy ordCrit _) = m ^. fileSelectionTagListModel . ordering
        btnText =
          case ordCrit of
            Alphabetic -> "ABC"
            Numeric -> "123"
     in styledButton_
          [resizeFactor (-1)]
          btnText
          (DoFileSelectionEvent CycleOrderCriteria)
  tagListOrderDirCycleButton =
    let (OrderBy _ ordDir) = m ^. fileSelectionTagListModel . ordering
     in styledButton_
          [resizeFactor (-1)]
          (T.pack . show $ ordDir)
          (DoFileSelectionEvent CycleOrderDirection)
  tagListLeaf (d, n) =
    hgrid_
      [childSpacing_ 0]
      [ draggable d . withStyleBasic [textRight, paddingR 1.5]
          . flip label_ []
          . descriptor
          $ d
      , withStyleBasic [paddingL 1.5, borderL 1 yuiBlack] . label . T.pack . show $ n
      ]

toggleViewSelectionButton :: TaggerWidget
toggleViewSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "View"
    (DoFileSelectionEvent ToggleSelectionView)

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
      . flip (|>) toggleFileEditMode
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
            , withNodeVisible (m ^. tagInputModel . isTagSelection) $
                styledButton_
                  [resizeFactor (-1)]
                  "Select"
                  (DoFileSelectionEvent TagSelectWholeChunk)
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
                  , neq taggingSelection
                  ]
                  || ( (m1 ^. tagInputModel . isTagSelection)
                        /= (m2 ^. tagInputModel . isTagSelection)
                     )
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
          . withStyleBasic
            [ textLeft
            , styleIf
                ( (m ^. tagInputModel . isTagSelection)
                    && HS.member fk (m ^. fileSelectionModel . taggingSelection)
                )
                ( bgColor
                    . modulateOpacity defaultElementOpacity
                    $ yuiOrange
                )
            ]
          $ hstack
            [ withNodeVisible (m ^. tagInputModel . isTagSelection)
                . withStyleBasic [border 1 yuiBlack]
                $ styledButton_
                  [resizeFactor (-1)]
                  ( if HS.member fk (m ^. fileSelectionModel . taggingSelection)
                      then "X"
                      else " "
                  )
                  (DoFileSelectionEvent $ TagSelect fk)
            , label_ fp [resizeFactor (-1)]
            ]
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
              . withStyleHover [textColor yuiWhite, bgColor yuiRed, border 1 yuiBlack]
              . withStyleBasic [textColor yuiRed, bgColor yuiLightPeach, border 1 yuiBlack]
              $ styledButton_
                [resizeFactor (-1)]
                "Confirm"
                (DoFileSelectionEvent . DeleteFileFromFileSystem $ fk)
          , withNodeVisible
              (not confirmDeleteVis)
              . tooltip_
                "Deletes the file from the database and file system."
                [tooltipDelay 500]
              . withStyleHover [textColor yuiWhite, bgColor yuiRed, border 1 yuiBlack]
              . withStyleBasic [textColor yuiRed, bgColor yuiLightPeach, border 1 yuiBlack]
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

shellCommandWidget :: TaggerModel -> TaggerWidget
shellCommandWidget ((^. fileSelectionModel . isMassOpMode) -> isMassOpModeIsTrue) =
  box_ [sizeReqUpdater (both (& fixed .~ 0))] $
    hstack
      [ withStyleBasic [bgColor yuiLightPeach]
          . tooltip_
            "If toggled, \
            \uses the entire selection as arguments to the given shell command."
            [tooltipDelay 1000]
          $ styledToggleButton_ [] "MassOp" (fileSelectionModel . isMassOpMode)
      , keystroke_
          [
            ( "Enter"
            , if isMassOpModeIsTrue
                then DoFileSelectionEvent RunSelectionShellCommand
                else DoFocusedFileEvent RunFocusedFileShellCommand
            )
          ]
          [ignoreChildrenEvts]
          . withStyleBasic [bgColor yuiLightPeach]
          . tooltip_
            "Run a shell command with the file(s) as arguments."
            [tooltipDelay 1000]
          . withStyleBasic
            [ minWidth 80
            , bgColor
                . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
                $ yuiLightPeach
            ]
          $ textField_ shellText []
      ]

tagListFilterTextField :: TaggerWidget
tagListFilterTextField =
  box_ [mergeRequired (\_ _ _ -> False)]
    . withStyleBasic [bgColor yuiLightPeach]
    . tooltip_
      "Filter the list of tag occurrences by a MetaDescriptor pattern"
      [tooltipDelay 1000]
    . withStyleBasic
      [ bgColor
          . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
          $ yuiLightPeach
      ]
    $ textFieldV
      mempty
      ( \t ->
          anonymousEvent
            [Event . DoFileSelectionEvent . IncludeTagListInfraToPattern $ t]
      )

fileSelectionChunkSizeNumField :: TaggerWidget
fileSelectionChunkSizeNumField =
  withStyleBasic
    [ maxWidth 80
    , bgColor
        . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
        $ yuiLightPeach
    ]
    $ numericField_ (fileSelectionModel . chunkSize) [minValue 0]

refreshFileSelectionButton :: TaggerWidget
refreshFileSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "Refresh"
    (DoFileSelectionEvent RefreshFileSelection)

shuffleSelectionButton :: TaggerWidget
shuffleSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "Shuffle"
    (DoFileSelectionEvent ShuffleSelection)

addFilesWidget :: TaggerModel -> TaggerWidget
addFilesWidget m =
  box_
    [ mergeRequired $ \_wenv x y ->
        (x ^. fileSelectionModel . addFileModel)
          /= (y ^. fileSelectionModel . addFileModel)
    ]
    $ zstack_
      [onlyTopActive]
      [ withNodeVisible
          (hasVis VisibilityMain $ m ^. fileSelectionModel . addFileModel . visibility)
          addFileMainVisWidget
      , withNodeVisible
          (hasVis VisibilityAlt $ m ^. fileSelectionModel . addFileModel . visibility)
          addFileAltVisWidget
      ]
 where
  addFileMainVisWidget =
    zstack_
      [onlyTopActive]
      [ withNodeVisible (m ^. fileSelectionModel . addFileModel . inProgress) $
          label_
            ( "Adding files from '"
                <> m ^. fileSelectionModel . addFileModel . input . text
                <> "'"
            )
            [resizeFactor (-1)]
      , withNodeVisible
          (not $ m ^. fileSelectionModel . addFileModel . inProgress)
          $ hstack_ [] [addFileTextField, scanDirectoriesButton]
      ]
   where
    addFileTextField =
      keystroke
        [ ("Enter", DoAddFileEvent AddFiles)
        , ("Up", NextHistory $ TaggerLens (fileSelectionModel . addFileModel . input))
        , ("Down", PrevHistory $ TaggerLens (fileSelectionModel . addFileModel . input))
        ]
        . withStyleBasic
          [ bgColor
              . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
              $ yuiLightPeach
          ]
        . tooltip_ "Enter to add file path" [tooltipDelay 1500]
        . withStyleBasic
          [ bgColor
              . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
              $ yuiLightPeach
          ]
        $ textField_
          (fileSelectionModel . addFileModel . input . text)
          [ onChange
              ( \t ->
                  if T.null t
                    then
                      Mempty $
                        TaggerLens
                          ( fileSelectionModel
                              . addFileModel
                              . input
                              . history
                              . historyIndex
                          )
                    else Unit ()
              )
          ]

    scanDirectoriesButton =
      styledButton_
        [resizeFactor (-1)]
        "Directories"
        (DoAddFileEvent ToggleAddFileVisibility)

  addFileAltVisWidget =
    zstack_
      [onlyTopActive]
      [ withNodeVisible
          (not $ m ^. fileSelectionModel . addFileModel . inProgress)
          $ vstack_ [] [directoryListWidget, closeScanDirectoriesButton]
      , withNodeVisible (m ^. fileSelectionModel . addFileModel . inProgress) $
          label_ "Adding Files..." [resizeFactor (-1)]
      ]
   where
    directoryListWidget =
      withStyleBasic
        [ bgColor
            . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
            $ yuiLightPeach
        ]
        . tooltip_ "Click a path to add its contents" [tooltipDelay 1500]
        . vscroll_ [wheelRate 50]
        . vstack_ []
        . map
          ( \dirPath ->
              styledButton_
                [resizeFactor (-1)]
                (T.pack dirPath)
                (DoAddFileEvent . AddFilePath $ dirPath)
          )
        $ m ^. fileSelectionModel . addFileModel . directoryList
    closeScanDirectoriesButton =
      styledButton_
        [resizeFactor (-1)]
        "Close"
        (DoAddFileEvent ToggleAddFileVisibility)

toggleFileEditMode :: TaggerWidget
toggleFileEditMode =
  styledButton_
    [resizeFactor (-1)]
    "Edit"
    ( ToggleVisibilityLabel
        (TaggerLens $ fileSelectionModel . fileSelectionVis)
        editFileMode
    )

editFileMode :: Text
editFileMode = "edit-file"

fileSelectionScrollWidgetNodeKey :: Text
fileSelectionScrollWidgetNodeKey = "file-selection-scroll-widget"