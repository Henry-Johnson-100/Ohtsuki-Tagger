{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Node.Micro where

import Data.List
import Data.Text hiding (foldl', intersperse, map)
import Database.Tagger.Type
import Monomer
import Node.Color
import Type.Config
import Type.Model

(!++) :: Text -> Text -> Text
(!++) = Data.Text.append

class GetPlainText g where
  getPlainText :: g -> Text

instance GetPlainText File where
  getPlainText = filePath

instance GetPlainText Descriptor where
  getPlainText = descriptor

instance GetPlainText FileWithTags where
  getPlainText = getPlainText . file

instance GetPlainText TagCount where
  getPlainText (d, n) = getPlainText d !++ " (" !++ (pack . show) n !++ ")"

stdDelayTooltip :: Text -> WidgetNode s e -> WidgetNode s e
stdDelayTooltip = flip tooltip_ [tooltipDelay 750]

stdScroll :: WidgetNode s e -> WidgetNode s e
stdScroll = scroll_ [wheelRate 50]

labeledWidget ::
  (WidgetModel s, WidgetEvent e) => Text -> WidgetNode s e -> WidgetNode s e
labeledWidget l w =
  box_ [alignLeft] . vstack_ [] $
    [label l `styleBasic` [textSize 16], w]

styledButton :: (WidgetModel s) => TaggerEvent -> Text -> WidgetNode s TaggerEvent
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
  (WidgetModel s, HasFileSelectionQuery s Text) => WidgetNode s TaggerEvent
queryTextField =
  dropTarget (FileSelectionAppendQuery . descriptor) $
    textField_ fileSelectionQuery []

descriptorNewTextField ::
  (WidgetModel s, HasNewDescriptorText s Text) => WidgetNode s TaggerEvent
descriptorNewTextField =
  textField_ newDescriptorText []

newFileTextField ::
  (WidgetModel s, HasNewFileText s Text) => WidgetNode s TaggerEvent
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
  (WidgetModel s, HasTagsString s Text) => WidgetNode s TaggerEvent
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
  (WidgetModel s, WidgetEvent e, HasQueryCriteria s QueryCriteria) =>
  WidgetNode s e
setQueryCriteriaDropdown =
  dropdown
    queryCriteria
    [ByTag, ByRelation, ByPattern, ByUntagged]
    (label . pack . show)
    (label . pack . show)

setArithmeticDropdown ::
  (WidgetModel s, WidgetEvent e, HasFileSetArithmetic s FileSetArithmetic) =>
  WidgetNode s e
setArithmeticDropdown =
  dropdown
    fileSetArithmetic
    [Union, Intersect, Diff]
    (label . pack . show)
    (label . pack . show)

taggingModeDropdown ::
  (WidgetModel s, WidgetEvent e, HasTaggingMode s TaggingMode) =>
  WidgetNode s e
taggingModeDropdown =
  dropdown taggingMode [TagMode, UntagMode] (label . pack . show) (label . pack . show)

commitQueryButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
commitQueryButton = styledButton FileSelectionCommitQuery "with"

shellCmdWidget :: (WidgetModel s, HasShellCmd s Text) => WidgetNode s TaggerEvent
shellCmdWidget =
  keystroke [("Enter", ShellCmd)]
    . hstack
    $ [shellCmdTextField, doShellCmdButton]

shellCmdTextField ::
  (WidgetModel s, HasShellCmd s Text) => WidgetNode s TaggerEvent
shellCmdTextField = textField_ shellCmd []

doShellCmdButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
doShellCmdButton = styledButton ShellCmd "Cmd"

resetDescriptorTreeToButton ::
  (WidgetModel s) =>
  Text ->
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
selectButton = flip styledButton "Select" . FileSelectionUpdate . (: [])

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
clearSelectionButton = styledButton FileSelectionClear "Clear"

appendToQueryButton :: WidgetModel s => Text -> WidgetNode s TaggerEvent
appendToQueryButton t = styledButton (FileSelectionAppendQuery t) "Add"

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
    . intersperse spacer
    . foldl' (\ws d -> ws ++ [draggableDescriptorWidget d]) []

draggableDescriptorWidget ::
  (WidgetModel s, WidgetEvent e) => Descriptor -> WidgetNode s e
draggableDescriptorWidget d =
  box_ [alignLeft]
    . draggable d
    . flip styleBasic [textColor blue]
    . flip label_ [ellipsis]
    . getPlainText
    $ d

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
      [resetDescriptorTreeToButton "#ALL#", parentDescriptorTreeButton, descriptorDeleteWidget]
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

treeLeafDescriptorWidget ::
  WidgetModel s =>
  Color ->
  Int ->
  Descriptor ->
  (Descriptor -> WidgetNode s TaggerEvent) ->
  WidgetNode s TaggerEvent
treeLeafDescriptorWidget tc l d a =
  hstack_ [] $
    [ label (Data.Text.replicate l "--" !++ "|"),
      draggable d $
        a d
          `styleBasic` [ textColor tc,
                         bgColor white,
                         border 0 white,
                         padding 0
                       ]
          `styleHover` [bgColor lightGray]
    ]

descriptorTreeWidget ::
  (WidgetModel s) =>
  DescriptorTree ->
  (Descriptor -> WidgetNode s TaggerEvent) ->
  WidgetNode s TaggerEvent
descriptorTreeWidget tr dAction =
  box . stdScroll . flip styleBasic [textFont "Regular"] . buildTreeWidget dAction $ tr
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