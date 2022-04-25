{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Node.Micro where

import qualified Data.List as L
import qualified Data.Text as T
import Database.Tagger.Type
import Monomer
import Node.Color
import Type.Config
import Type.Model

(!++) :: T.Text -> T.Text -> T.Text
(!++) = T.append

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

styledButton :: (WidgetModel s) => TaggerEvent -> T.Text -> WidgetNode s TaggerEvent
styledButton a t =
  button t a

buttonStylingBasic :: [StyleState]
buttonStylingBasic = [bgColor white, border 0 white]

buttonStylingHover :: [StyleState]
buttonStylingHover = [bgColor lightGray]

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
  (WidgetModel s, HasFileSelectionModel s a1, HasQueryText a1 T.Text) =>
  WidgetNode s TaggerEvent
queryTextField =
  dropTarget (DoFileSelectionEvent . FileSelectionAppendToQueryText . descriptor) $
    textField_ (fileSelectionModel . queryText) []

descriptorNewTextField ::
  (WidgetModel s, HasNewDescriptorText s T.Text) => WidgetNode s TaggerEvent
descriptorNewTextField =
  textField_ newDescriptorText []

newFileTextField ::
  (WidgetModel s, HasNewFileText s T.Text) => WidgetNode s TaggerEvent
newFileTextField = textField newFileText

selectionDisplayParentsNumberField ::
  (WidgetModel s, HasProgramConfig s TaggerConfig) =>
  WidgetNode s TaggerEvent
selectionDisplayParentsNumberField =
  numericField
    (programConfig . selectionconf . selectionDisplayParents)

newFileTextCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
newFileTextCommitButton = styledButton NewFileTextCommit "Add Path"

tagsStringTextField ::
  (WidgetModel s, HasTagsString s T.Text) => WidgetNode s TaggerEvent
tagsStringTextField =
  dropTarget (TagsStringAppend . descriptor) $ textField_ tagsString []

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

descriptorDeleteWidget :: WidgetModel s => WidgetNode s TaggerEvent
descriptorDeleteWidget =
  box_ []
    . dropTarget DescriptorDelete
    . styledButton (IOEvent ())
    $ "X"

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

commitQueryButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
commitQueryButton =
  styledButton
    (DoFileSelectionEvent FileSelectionCommitQueryText)
    "with"

shellCmdWidget :: (WidgetModel s, HasShellCmd s T.Text) => WidgetNode s TaggerEvent
shellCmdWidget =
  keystroke [("Enter", ShellCmd)]
    . hstack
    $ [shellCmdTextField, doShellCmdButton]

shellCmdTextField ::
  (WidgetModel s, HasShellCmd s T.Text) => WidgetNode s TaggerEvent
shellCmdTextField = textField_ shellCmd []

doShellCmdButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
doShellCmdButton = styledButton ShellCmd "Cmd"

resetDescriptorTreeToButton ::
  (WidgetModel s) =>
  T.Text ->
  WidgetNode s TaggerEvent
resetDescriptorTreeToButton t = styledButton (RequestDescriptorTree t) "↺"

resetUnrelatedDescriptorTree :: (WidgetModel s) => WidgetNode s TaggerEvent
resetUnrelatedDescriptorTree = styledButton RefreshUnrelatedDescriptorTree "↺"

parentDescriptorTreeButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
parentDescriptorTreeButton = styledButton DescriptorTreePutParent "↑"

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

appendToQueryButton :: WidgetModel s => T.Text -> WidgetNode s TaggerEvent
appendToQueryButton t =
  styledButton
    ( DoFileSelectionEvent . FileSelectionAppendToQueryText $
        t
    )
    "Add"

treeLeafButtonRequestDescriptorTree ::
  WidgetModel s =>
  Descriptor ->
  WidgetNode s TaggerEvent
treeLeafButtonRequestDescriptorTree d =
  styledButton (RequestDescriptorTree . descriptor $ d) (descriptor d)

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

explorableDescriptorTreeWidget ::
  WidgetModel s =>
  DescriptorTree ->
  WidgetNode s TaggerEvent
explorableDescriptorTreeWidget tr =
  dropTarget
    ( \d' ->
        maybe
          (IOEvent ())
          (\m' -> DescriptorCreateRelation [m'] [d'])
          (getNode tr)
    )
    $ generalDescriptorTreeWidget
      tr
      [ resetDescriptorTreeToButton "#ALL#",
        parentDescriptorTreeButton,
        descriptorDeleteWidget
      ]
      treeLeafButtonRequestDescriptorTree

unrelatedDescriptorTreeWidget ::
  WidgetModel s =>
  DescriptorTree ->
  WidgetNode s TaggerEvent
unrelatedDescriptorTreeWidget tr =
  dropTarget (\d' -> DescriptorUnrelate [d']) $
    generalDescriptorTreeWidget
      tr
      [resetUnrelatedDescriptorTree]
      treeLeafButtonRequestDescriptorTree -- #TODO change this

generalDescriptorTreeWidget ::
  WidgetModel s =>
  DescriptorTree ->
  [WidgetNode s TaggerEvent] ->
  (Descriptor -> WidgetNode s TaggerEvent) ->
  WidgetNode s TaggerEvent
generalDescriptorTreeWidget tr bs dAction =
  flip styleBasic [border 1 black] . box_ [alignTop, alignLeft] $
    hstack_
      []
      [ vsplit (vstack_ [] bs, spacer),
        separatorLine,
        descriptorTreeWidget (sortChildren tr) dAction
      ]
  where
    descriptorTreeWidget ::
      (WidgetModel s) =>
      DescriptorTree ->
      (Descriptor -> WidgetNode s TaggerEvent) ->
      WidgetNode s TaggerEvent
    descriptorTreeWidget tr dAction =
      box . stdScroll . flip styleBasic [textFont "Regular"]
        . buildTreeWidget dAction
        $ tr
      where
        buildTreeWidget ::
          (WidgetModel s) =>
          (Descriptor -> WidgetNode s TaggerEvent) ->
          DescriptorTree ->
          WidgetNode s TaggerEvent
        buildTreeWidget action = buildTreeWidgetAccum 0 (vstack []) action
          where
            buildTreeWidgetAccum ::
              (WidgetModel s) =>
              Int ->
              WidgetNode s TaggerEvent ->
              (Descriptor -> WidgetNode s TaggerEvent) ->
              DescriptorTree ->
              WidgetNode s TaggerEvent
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
          WidgetModel s =>
          Color ->
          Int ->
          Descriptor ->
          (Descriptor -> WidgetNode s TaggerEvent) ->
          WidgetNode s TaggerEvent
        treeLeafDescriptorWidget tc l d a =
          hstack_ [] $
            [ label (T.replicate l "--" !++ "|"),
              draggable d $
                a d
                  `styleBasic` [ textColor tc,
                                 bgColor white,
                                 border 0 white,
                                 padding 0
                               ]
                  `styleHover` [bgColor lightGray]
            ]

imageDetailWidget ::
  (WidgetModel s) =>
  Bool ->
  [FileWithTags] ->
  [TagCount] ->
  WidgetNode s TaggerEvent
imageDetailWidget isSoloTagMode' currentFileSelection' tcs' =
  flip styleBasic [borderL 1 black, rangeWidth 160 800]
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
                      $ tcs'
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
