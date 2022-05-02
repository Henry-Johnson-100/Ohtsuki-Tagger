{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Node.Micro where

import Control.Lens
import qualified Data.List as L
import qualified Data.Text as T
import Database.Tagger.Type
import Monomer
import Node.Color
import Type.BufferList
import Type.Config
import Type.Model

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

(!++) :: T.Text -> T.Text -> T.Text
(!++) = T.append

getPathComponents :: Int -> T.Text -> T.Text
getPathComponents n p =
  let !brokenPath = T.splitOn "/" p
      !droppedDirs = length brokenPath - n
   in (!++) ((T.pack . show) droppedDirs !++ ".../")
        . T.intercalate "/"
        . drop droppedDirs
        $ brokenPath

class GetPlainText g where
  getPlainText :: g -> T.Text

instance GetPlainText File where
  getPlainText = filePath

instance GetPlainText Descriptor where
  getPlainText = descriptor

instance GetPlainText FileWithTags where
  getPlainText = getPlainText . file

instance GetPlainText TagCount where
  getPlainText (d, n) = getPlainText d !++ " (" !++ (T.pack . show) n !++ ")"

stdDelayTooltip :: T.Text -> WidgetNode s e -> WidgetNode s e
stdDelayTooltip = flip tooltip_ [tooltipDelay 750]

stdScroll :: WidgetNode s e -> WidgetNode s e
stdScroll = scroll_ [wheelRate 50]

labeledWidget ::
  (WidgetModel s, WidgetEvent e) => T.Text -> WidgetNode s e -> WidgetNode s e
labeledWidget l w =
  box_ [alignLeft] . vstack_ [] $
    [label l `styleBasic` [textSize 16], w]

{-
 ____  _   _ _____ _____ ___  _   _ ____
| __ )| | | |_   _|_   _/ _ \| \ | / ___|
|  _ \| | | | | |   | || | | |  \| \___ \
| |_) | |_| | | |   | || |_| | |\  |___) |
|____/ \___/  |_|   |_| \___/|_| \_|____/
-}

styledButton :: (WidgetModel s) => TaggerEvent -> T.Text -> WidgetNode s TaggerEvent
styledButton a t =
  button t a

buttonStylingBasic :: [StyleState]
buttonStylingBasic = [bgColor white, border 0 white]

buttonStylingHover :: [StyleState]
buttonStylingHover = [bgColor lightGray]

lazyBufferLoadButton :: WidgetNode TaggerModel TaggerEvent
lazyBufferLoadButton = styledButton (DoFileSelectionEvent LazyBufferLoad) "Load"

lazyBufferLoadAllButton :: WidgetNode TaggerModel TaggerEvent
lazyBufferLoadAllButton = styledButton (DoFileSelectionEvent LazyBufferLoadAll) "All"

lazyBufferFlushButton :: WidgetNode TaggerModel TaggerEvent
lazyBufferFlushButton = styledButton (DoFileSelectionEvent LazyBufferFlush) "Flush"

newFileTextCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
newFileTextCommitButton = styledButton NewFileTextCommit "Add Path"

tagCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
tagCommitButton = styledButton TagCommitTagsString "with"

descriptorNewCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
descriptorNewCommitButton =
  styledButton DescriptorCommitNewDescriptorText "New Descriptor"

initializeDatabaseButton :: (WidgetModel s) => WidgetNode s TaggerEvent
initializeDatabaseButton =
  styledButton DatabaseInitialize "Initialize Database"
    `styleBasic` [bgColor (Color 212 0 0 0.83)]

databaseConnectButton :: (WidgetModel s) => WidgetNode s TaggerEvent
databaseConnectButton = styledButton DatabaseConnect "Connect"

databaseBackupButton :: WidgetModel s => WidgetNode s TaggerEvent
databaseBackupButton = styledButton DatabaseBackup "Backup"

configurationExportButton :: (WidgetModel s) => WidgetNode s TaggerEvent
configurationExportButton =
  styledButton (DoConfigurationEvent ExportAll) "Export Configuration"

toggleConfigConfigureVisibility :: (WidgetModel s) => WidgetNode s TaggerEvent
toggleConfigConfigureVisibility = styledButton (ToggleVisibilityMode Config) "Config"

toggleDatabaseConfigureVisibility :: (WidgetModel s) => WidgetNode s TaggerEvent
toggleDatabaseConfigureVisibility =
  styledButton (ToggleVisibilityMode Database) "Database"

toggleSelectionConfigureVisibility :: (WidgetModel s) => WidgetNode s TaggerEvent
toggleSelectionConfigureVisibility =
  styledButton
    (ToggleVisibilityMode Selection)
    "Selection"

toggleDescriptorConfigureVisibility :: WidgetNode TaggerModel TaggerEvent
toggleDescriptorConfigureVisibility =
  styledButton
    (ToggleVisibilityMode ProgramVisibilityDescriptor)
    "Descriptor"

descriptorDeleteWidget :: WidgetModel s => WidgetNode s TaggerEvent
descriptorDeleteWidget =
  box_ []
    . dropTarget DescriptorDelete
    . styledButton (IOEvent ())
    $ "X"

commitQueryButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
commitQueryButton =
  styledButton
    (DoFileSelectionEvent FileSelectionCommitQueryText)
    "with"

doShellCmdButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
doShellCmdButton = styledButton ShellCmd "Cmd"

resetDescriptorTreeToButton ::
  (WidgetModel s) =>
  T.Text ->
  WidgetNode s TaggerEvent
resetDescriptorTreeToButton t =
  styledButton
    ( DoDescriptorEvent . RequestDescriptorTree mainDescriptorTree $ t
    -- RequestDescriptorTree t
    )
    "↺"

resetUnrelatedDescriptorTree :: WidgetNode TaggerModel TaggerEvent
resetUnrelatedDescriptorTree =
  styledButton
    (DoDescriptorEvent (RefreshDescriptorTree unrelatedDescriptorTree))
    -- RefreshUnrelatedDescriptorTree
    "↺"

parentDescriptorTreeButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
parentDescriptorTreeButton =
  styledButton
    (DoDescriptorEvent (DescriptorTreePutParent mainDescriptorTree))
    -- DescriptorTreePutParent
    "↑"

selectButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
selectButton =
  flip styledButton "Select"
    . DoFileSelectionEvent
    . FileSelectionUpdate
    . (: [])

previewButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
previewButton = flip styledButton "Preview" . DoSingleFileEvent . SingleFilePut

fileSingleNextFromFileSelectionButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
fileSingleNextFromFileSelectionButton =
  styledButton (DoSingleFileEvent SingleFileNextFromFileSelection) "↓"

fileSinglePrevFromFileSelectionButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
fileSinglePrevFromFileSelectionButton =
  styledButton (DoSingleFileEvent SingleFilePrevFromFileSelection) "↑"

clearSelectionButton ::
  (WidgetModel s) =>
  WidgetNode s TaggerEvent
clearSelectionButton =
  styledButton
    (DoFileSelectionEvent FileSelectionClear)
    "Clear"

fileSelectionShuffleButton :: WidgetNode TaggerModel TaggerEvent
fileSelectionShuffleButton =
  styledButton
    (DoFileSelectionEvent FileSelectionShuffle)
    "Shuffle"

appendToQueryButton :: WidgetModel s => T.Text -> WidgetNode s TaggerEvent
appendToQueryButton t =
  styledButton
    ( DropTargetAppendText_ (fileSelectionModel . queryText) id $
        t
    )
    "Add"

treeLeafButtonRequestDescriptorTree ::
  WidgetModel s =>
  Descriptor ->
  WidgetNode s TaggerEvent
treeLeafButtonRequestDescriptorTree d =
  styledButton
    ( (DoDescriptorEvent . RequestDescriptorTree mainDescriptorTree)
        . descriptor
        $ d
    )
    (descriptor d)

{-
 _____ _______  _______ _____ ___ _____ _     ____  ____
|_   _| ____\ \/ /_   _|  ___|_ _| ____| |   |  _ \/ ___|
  | | |  _|  \  /  | | | |_   | ||  _| | |   | | | \___ \
  | | | |___ /  \  | | |  _|  | || |___| |___| |_| |___) |
  |_| |_____/_/\_\ |_| |_|   |___|_____|_____|____/|____/
-}

descriptorTreeConfigureMainRequestTextField :: WidgetNode TaggerModel TaggerEvent
descriptorTreeConfigureMainRequestTextField =
  textField (programConfig . descriptorTreeConf . descriptorTreeMainRequest)

dbPathTextField ::
  (WidgetModel s, HasProgramConfig s TaggerConfig) =>
  WidgetNode s TaggerEvent
dbPathTextField = textField (programConfig . dbconf . dbconfPath)

dbBackupTextField ::
  (WidgetModel s, HasProgramConfig s TaggerConfig) => WidgetNode s TaggerEvent
dbBackupTextField = textField (programConfig . dbconf . dbconfBackup)

dbAutoConnectCheckBox ::
  (WidgetModel s, HasProgramConfig s TaggerConfig) =>
  WidgetNode s TaggerEvent
dbAutoConnectCheckBox =
  labeledCheckbox
    "Auto-Connect"
    (programConfig . dbconf . dbconfAutoConnect)

queryTextField ::
  WidgetNode TaggerModel TaggerEvent
queryTextField =
  dropTarget (DropTargetAppendText_ (fileSelectionModel . queryText) descriptor) $
    textField_ (fileSelectionModel . queryText) []

descriptorNewTextField ::
  (WidgetModel s, HasNewDescriptorText s T.Text) => WidgetNode s TaggerEvent
descriptorNewTextField =
  textField_ newDescriptorText []

newFileTextField ::
  (WidgetModel s, HasNewFileText s T.Text) => WidgetNode s TaggerEvent
newFileTextField = textField newFileText

selectionDisplayParentsNumberField ::
  WidgetNode TaggerModel TaggerEvent
selectionDisplayParentsNumberField =
  labeledWidget "Display Parent Folders"
    . flip styleBasic [textLeft]
    . numericField
    $ (programConfig . selectionconf . selectionDisplayParents)

selectionDisplayBufferSizeNumberField ::
  WidgetNode TaggerModel TaggerEvent
selectionDisplayBufferSizeNumberField =
  labeledWidget "Selection Buffer Size"
    . flip styleBasic [textLeft]
    . numericField
    $ (programConfig . selectionconf . selectionBufferSize)

tagsStringTextField ::
  WidgetNode TaggerModel TaggerEvent
tagsStringTextField =
  dropTarget (DropTargetAppendText_ tagsString descriptor) $ textField_ tagsString []

shellCmdTextField ::
  (WidgetModel s, HasShellCmd s T.Text) => WidgetNode s TaggerEvent
shellCmdTextField = textField_ shellCmd []

renameDescriptorWidget :: WidgetNode TaggerModel TaggerEvent
renameDescriptorWidget =
  box_ []
    . labeledWidget "Rename Descriptor"
    . keystroke_
      [("Enter", DoDescriptorEvent RenameDescriptor)]
      []
    . hstack_ []
    $ [ dropTarget
          ( DropTargetAppendText_
              (descriptorModel . renameDescriptorFrom)
              descriptor
          )
          . textField
          $ (descriptorModel . renameDescriptorFrom),
        styledButton (DoDescriptorEvent RenameDescriptor) "To",
        dropTarget
          ( DropTargetAppendText_
              (descriptorModel . renameDescriptorTo)
              descriptor
          )
          . textField
          $ (descriptorModel . renameDescriptorTo)
      ]

setQueryCriteriaDropdown ::
  WidgetNode TaggerModel TaggerEvent
setQueryCriteriaDropdown =
  dropdown
    (fileSelectionModel . queryCriteria)
    [ByTag, ByRelation, ByPattern, ByUntagged]
    (label . T.pack . show)
    (label . T.pack . show)

setArithmeticDropdown ::
  WidgetNode TaggerModel TaggerEvent
setArithmeticDropdown =
  dropdown
    (fileSelectionModel . setArithmetic)
    [Union, Intersect, Diff]
    (label . T.pack . show)
    (label . T.pack . show)

taggingModeDropdown ::
  (WidgetModel s, WidgetEvent e, HasTaggingMode s TaggingMode) =>
  WidgetNode s e
taggingModeDropdown =
  dropdown
    taggingMode
    [TagMode, UntagMode]
    (label . T.pack . show)
    (label . T.pack . show)

shellCmdWidget :: (WidgetModel s, HasShellCmd s T.Text) => WidgetNode s TaggerEvent
shellCmdWidget =
  keystroke [("Enter", ShellCmd)]
    . hstack
    $ [shellCmdTextField, doShellCmdButton]

draggableDescriptorListWidget ::
  (WidgetModel s, WidgetEvent e) => [Descriptor] -> WidgetNode s e
draggableDescriptorListWidget =
  box_ [alignLeft]
    . hstack
    . L.intersperse spacer
    . L.foldl' (\ws d -> ws ++ [draggableDescriptorWidget d]) []

draggableDescriptorWidget ::
  (WidgetModel s, WidgetEvent e) => Descriptor -> WidgetNode s e
draggableDescriptorWidget d =
  box_ [alignLeft]
    . draggable d
    . flip styleBasic [textColor blue]
    . flip label_ [ellipsis]
    . getPlainText
    $ d

mainDescriptorTreeWidget ::
  DescriptorTreeConfig ->
  DescriptorTree ->
  WidgetNode TaggerModel TaggerEvent
mainDescriptorTreeWidget dtrConf tr =
  dropTarget
    ( \d' ->
        maybe
          (IOEvent ())
          (\m' -> DescriptorCreateRelation [m'] [d'])
          (getNode tr)
    )
    $ generalDescriptorTreeWidget
      tr
      [ resetDescriptorTreeToButton (dtrConf ^. descriptorTreeMainRequest),
        parentDescriptorTreeButton,
        descriptorDeleteWidget
      ]
      treeLeafButtonRequestDescriptorTree
      dtrConf

unrelatedDescriptorTreeWidget ::
  DescriptorTreeConfig ->
  DescriptorTree ->
  WidgetNode TaggerModel TaggerEvent
unrelatedDescriptorTreeWidget dtrConf tr =
  dropTarget (\d' -> DescriptorUnrelate [d']) $
    generalDescriptorTreeWidget
      tr
      [resetUnrelatedDescriptorTree]
      (label . getPlainText)
      dtrConf

generalDescriptorTreeWidget ::
  DescriptorTree ->
  [WidgetNode TaggerModel TaggerEvent] ->
  (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
  DescriptorTreeConfig ->
  WidgetNode TaggerModel TaggerEvent
generalDescriptorTreeWidget tr bs dAction dtrConf =
  flip styleBasic [border 1 black] . box_ [alignTop, alignLeft] $
    hstack_
      []
      [ vsplit (vstack_ [] bs, spacer),
        separatorLine,
        descriptorTreeWidget (sortChildren tr) dAction
      ]
  where
    descriptorTreeWidget ::
      DescriptorTree ->
      (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
      WidgetNode TaggerModel TaggerEvent
    descriptorTreeWidget tr dAction =
      box . stdScroll . flip styleBasic [textFont "Regular"]
        . buildTreeWidget dAction
        $ tr
      where
        buildTreeWidget ::
          (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
          DescriptorTree ->
          WidgetNode TaggerModel TaggerEvent
        buildTreeWidget action = buildTreeWidgetAccum 0 (vstack []) action
          where
            buildTreeWidgetAccum ::
              Int ->
              WidgetNode TaggerModel TaggerEvent ->
              (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
              DescriptorTree ->
              WidgetNode TaggerModel TaggerEvent
            buildTreeWidgetAccum l acc action tr =
              case tr of
                NullTree -> acc
                Infra d ->
                  vstack
                    [ acc,
                      treeLeafDescriptorWidget
                        black
                        l
                        d
                        action
                    ]
                Meta d cs ->
                  appendVStack
                    ( vstack
                        [ acc,
                          hstack
                            [ treeLeafDescriptorWidget
                                yuiBlue
                                l
                                d
                                action
                            ]
                        ]
                    )
                    ( vstack $
                        map
                          ( \c ->
                              case c of
                                Infra d' ->
                                  treeLeafDescriptorWidget
                                    black
                                    (l + 1)
                                    d'
                                    action
                                Meta d' _ ->
                                  treeLeafDescriptorWidget
                                    yuiBlue
                                    (l + 1)
                                    d'
                                    action
                                NullTree ->
                                  spacer
                                    `styleBasic` [padding 0, border 0 white]
                          )
                          cs
                    )
        appendVStack x y = vstack [x, y]
        treeLeafDescriptorWidget ::
          Color ->
          Int ->
          Descriptor ->
          (Descriptor -> WidgetNode TaggerModel TaggerEvent) ->
          WidgetNode TaggerModel TaggerEvent
        treeLeafDescriptorWidget tc l d a =
          hstack_ [] $
            [ label (T.replicate l "--" !++ "|"),
              dropTarget_
                (DoDescriptorEvent . flip RepresentativeCreate d . file)
                [ dropTargetStyle
                    [border 1 black]
                ]
                . draggable d
                . flip
                  styleBasic
                  [ textColor tc,
                    bgColor white,
                    border 0 white,
                    padding 0
                  ]
                . flip
                  styleHover
                  [bgColor lightGray]
                . a
                $ d
            ]

imageDetailWidget ::
  TaggerModel -> TaggerWidget
imageDetailWidget m =
  let !isSoloTagMode' = m ^. doSoloTag
      !currentFileSelection' = cCollect (m ^. fileSelectionModel . fileSelection)
      !tagCounts' = m ^. singleFileModel . tagCounts
   in flip styleBasic [borderL 1 black, rangeWidth 160 800]
        . box_ [alignLeft]
        . vstack_ []
        $ [ label "Details:",
            spacer,
            label "Solo Tagging Mode"
              `styleBasic` [textColor yuiOrange]
              `nodeVisible` isSoloTagMode',
            spacer,
            vsplit_
              [splitIgnoreChildResize True]
              ( vstack_
                  []
                  [ label "Tags:",
                    hstack_
                      []
                      [ spacer,
                        flip styleBasic [border 1 black]
                          . vscroll_ [wheelRate 50]
                          . vstack_ []
                          . map imageDetailDescriptor
                          . L.sort
                          $ tagCounts'
                      ]
                  ],
                vstack_
                  []
                  [ label $
                      "In Selection: "
                        !++ "("
                        !++ (T.pack . show . length) currentFileSelection'
                        !++ ")",
                    spacer,
                    hstack_
                      []
                      [ spacer,
                        flip styleBasic [border 1 black]
                          . vscroll_ [wheelRate 50]
                          . vstack_ []
                          . map imageDetailDescriptor
                          . L.sort
                          . sumSelectionTagCounts
                          $ currentFileSelection'
                      ]
                  ]
              )
          ]
  where
    imageDetailDescriptor ::
      (WidgetModel s) =>
      TagCount ->
      WidgetNode s TaggerEvent
    imageDetailDescriptor (d, c) =
      hgrid_ [] $
        [ draggable_ d []
            . box_ [alignLeft]
            . flip styleBasic [textColor yuiBlue]
            . label
            . getPlainText
            $ d,
          flip styleBasic [paddingL 15]
            . box_ [alignLeft]
            . flip styleBasic [textColor yuiBlue]
            . label
            . T.pack
            . show
            $ c
        ]
    sumSelectionTagCounts :: [FileWithTags] -> [TagCount]
    sumSelectionTagCounts [] = []
    sumSelectionTagCounts xs =
      tagCountUnmap
        . L.foldl1' tagCountMapSumUnion
        . map fileWithTagsToTagCountMap
        $ xs

representativeFilePreview :: Maybe Representative -> TaggerWidget
representativeFilePreview mr =
  labeledWidget
    ( maybe
        "No Representative"
        ( (!++) "Representative File for: "
            . descriptor
            . repDescriptorId
        )
        mr
    )
    . flip styleBasic [border 1 black]
    . dropTarget_
      (DoDescriptorEvent . RepresentativeFileLookup)
      [dropTargetStyle [border 2 yuiYellow]]
    . box_ []
    . maybe
      (label "")
      (flip image_ [fitHeight, alignCenter] . filePath . repFileId)
    $ mr
