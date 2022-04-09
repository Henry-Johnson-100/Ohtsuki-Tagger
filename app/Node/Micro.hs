{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

module Node.Micro where

import Data.Text
import Database.Tagger.Type
import Monomer
import Node.Color
import Type.Model

class GetPlainText g where
  getPlainText :: g -> Text

instance GetPlainText File where
  getPlainText = filePath

instance GetPlainText Descriptor where
  getPlainText = descriptor

instance GetPlainText FileWithTags where
  getPlainText = getPlainText . file

stdDelayTooltip :: Text -> WidgetNode s e -> WidgetNode s e
stdDelayTooltip = flip tooltip_ [tooltipDelay 750]

stdScroll :: WidgetNode s e -> WidgetNode s e
stdScroll = scroll_ [wheelRate 50]

styledButton :: (WidgetModel s) => TaggerEvent -> Text -> WidgetNode s TaggerEvent
styledButton a t =
  button t a
    `styleBasic` [bgColor bgDefault, border 0 bgDefault]
    `styleHover` [bgColor bgLightGray]

queryTextField ::
  (WidgetModel s, HasFileSelectionQuery s Text) => WidgetNode s TaggerEvent
queryTextField = textField_ fileSelectionQuery []

tagsStringTextField ::
  (WidgetModel s, HasTagsString s Text) => WidgetNode s TaggerEvent
tagsStringTextField = textField_ tagsString []

tagCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
tagCommitButton = styledButton TagCommitTagsString "Tag With"

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

commitQueryButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
commitQueryButton = styledButton FileSelectionCommitQuery "Query"

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

resetDescriptorTreeButton ::
  (WidgetModel s) =>
  WidgetNode s TaggerEvent
resetDescriptorTreeButton = styledButton (RequestDescriptorTree "#ALL#") "Top"

selectButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
selectButton = flip styledButton "Select" . FileSelectionUpdate . (: [])

previewButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
previewButton = flip styledButton "Preview" . FileSinglePut

clearSelectionButton ::
  (WidgetModel s) =>
  WidgetNode s TaggerEvent
clearSelectionButton = styledButton FileSelectionClear "CS"

appendToQueryButton :: WidgetModel s => Text -> WidgetNode s TaggerEvent
appendToQueryButton t = styledButton (FileSelectionAppendQuery t) "Add"

draggableDescriptorWidget ::
  (WidgetModel s, WidgetEvent e) => Descriptor -> WidgetNode s e
draggableDescriptorWidget d =
  draggable d . flip styleBasic [textColor textBlue] . label . getPlainText $ d