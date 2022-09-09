{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use ||" #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.Selection (
  widget,
  fileSelectionScrollWidgetNodeKey,
) where

import Control.Lens ((&), (.~), (^.))
import Data.Event (
  FileSelectionEvent (
    AddFiles,
    ClearSelection,
    CycleTagOrderCriteria,
    CycleTagOrderDirection,
    DeleteFileFromFileSystem,
    DoFileSelectionWidgetEvent,
    NextAddFileHist,
    PrevAddFileHist,
    RefreshFileSelection,
    RemoveFileFromDatabase,
    RemoveFileFromSelection,
    RenameFile,
    ResetAddFileHistIndex,
    RunSelectionShellCommand,
    ShuffleSelection,
    TogglePaneVisibility,
    ToggleSelectionView
  ),
  FileSelectionWidgetEvent (CycleNextChunk, CyclePrevChunk),
  FocusedFileEvent (RunFocusedFileShellCommand),
  TaggerEvent (DoFileSelectionEvent, DoFocusedFileEvent, IOEvent),
 )
import qualified Data.List as L
import Data.Model.Core (TaggerModel, getSelectionChunk)
import Data.Model.Lens (
  HasAddFileText (addFileText),
  HasChunkSequence (chunkSequence),
  HasChunkSize (chunkSize),
  HasCurrentChunk (currentChunk),
  HasDeleteFileIsVis (deleteFileIsVis),
  HasFileInfoRenameText (fileInfoRenameText),
  HasFileSelectionInfoMap (fileSelectionInfoMap),
  HasFileSelectionModel (fileSelectionModel),
  HasFileSelectionVis (fileSelectionVis),
  HasIsMassOpMode (isMassOpMode),
  HasSelection (selection),
  HasSetOp (setOp),
  HasShellText (shellText),
  HasTagOccurrences (tagOccurrences),
  HasTagOrdering (tagOrdering),
  fileInfoAt,
 )
import Data.Model.Shared.Core (
  OrderBy (OrderBy),
  OrderCriteria (Alphabetic, Numeric),
  OrderDirection (Asc, Desc),
  Visibility (VisibilityAlt, VisibilityLabel),
  hasVis,
 )
import qualified Data.OccurrenceHashMap as OHM
import qualified Data.Ord as O
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Tagger (SetOp (Difference, Intersect, Union))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (Descriptor (descriptor), File (File))
import Interface.Theme (yuiLightPeach, yuiRed)
import Interface.Widget.Internal.Core (
  styledButton_,
  withNodeKey,
  withNodeVisible,
  withStyleBasic,
  withStyleHover,
 )
import Interface.Widget.Internal.Type (TaggerWidget)
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
  black,
  box_,
  draggable,
  dropdown,
  hgrid_,
  hstack,
  hstack_,
  keystroke,
  keystroke_,
  label,
  label_,
  numericField_,
  separatorLine,
  textField_,
  toggleButton_,
  tooltipDelay,
  tooltip_,
  vscroll_,
  vstack_,
  white,
  zstack,
  zstack_,
 )
import Monomer.Core.Lens (fixed)
import Util (both)

widget :: TaggerModel -> TaggerWidget
widget m =
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
      , setOpDrowpdown
      ]

setOpDrowpdown :: TaggerWidget
setOpDrowpdown =
  dropdown
    (fileSelectionModel . setOp)
    [Union, Intersect, Difference]
    (flip label_ [resizeFactor (-1)] . T.pack . show)
    (flip label_ [resizeFactor (-1)] . T.pack . show)

clearSelectionButton :: TaggerWidget
clearSelectionButton =
  styledButton_
    [resizeFactor (-1)]
    "Clear"
    (DoFileSelectionEvent ClearSelection)

selectionSizeLabel :: TaggerModel -> TaggerWidget
selectionSizeLabel m =
  flip label_ [resizeFactor (-1)] $
    "In Selection: ("
      <> ( T.pack . show
            . Seq.length
            $ m ^. fileSelectionModel . selection
         )
      <> ")"

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

toggleFileEditMode :: TaggerWidget
toggleFileEditMode =
  styledButton_
    [resizeFactor (-1)]
    "Edit"
    (DoFileSelectionEvent (TogglePaneVisibility editFileMode))

editFileMode :: Text
editFileMode = "edit-file"

fileSelectionScrollWidgetNodeKey :: Text
fileSelectionScrollWidgetNodeKey = "file-selection-scroll-widget"