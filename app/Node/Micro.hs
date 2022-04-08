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

stdDelayTooltip :: Text -> WidgetNode s e -> WidgetNode s e
stdDelayTooltip = flip tooltip_ [tooltipDelay 750]

stdScroll :: WidgetNode s e -> WidgetNode s e
stdScroll = scroll_ [wheelRate 50]

styledButton :: (WidgetModel s) => TaggerEvent -> Text -> WidgetNode s TaggerEvent
styledButton a t =
  button t a
    `styleBasic` [bgColor bgDefault, border 0 bgDefault]
    `styleHover` [bgColor bgLightGray]

queryWidget ::
  ( WidgetModel s,
    HasFileSelectionQuery s Text,
    HasQueryCriteria s QueryCriteria,
    HasFileSetArithmetic s FileSetArithmetic
  ) =>
  WidgetNode s TaggerEvent
queryWidget =
  keystroke [("Enter", FileSelectionCommitQuery)] $
    hgrid_
      []
      [ queryTextField,
        box_ [] $
          hstack_ [] [setQueryCriteriaDropdown, setArithmeticDropdown, commitQueryButton]
      ]
  where
    queryTextField ::
      (WidgetModel s, HasFileSelectionQuery s Text) => WidgetNode s TaggerEvent
    queryTextField = textField_ fileSelectionQuery []

    setQueryCriteriaDropdown ::
      (WidgetModel s, WidgetEvent e, HasQueryCriteria s QueryCriteria) =>
      WidgetNode s e
    setQueryCriteriaDropdown =
      dropdown
        queryCriteria
        [ByTag, ByRelation, ByPattern]
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
