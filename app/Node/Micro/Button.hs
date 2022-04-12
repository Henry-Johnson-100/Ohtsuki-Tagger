{-# LANGUAGE OverloadedStrings #-}

module Node.Micro.Button
  ( requestDescriptorTreeButton,
    requestDescriptorTreeButtonForTreeLeaf,
    refreshUnrelatedDescriptorTreeButton,
    descriptorTreePutParentButton,
    descriptorDeleteDroppableButton,
    tagCommitTagsStringButton,
    descriptorCommitNewDescriptorTextButton,
    shellCmdButton,
    fileSelectionUpdateButton,
    fileSelectionCommitQueryButton,
    fileSinglePutButton,
    fileSelectionClearButton,
  )
where

import qualified Data.Text as T
import Database.Tagger.Type
import Monomer
import Node.Micro.Colors
import Type.Model

buttonStdStyle :: (WidgetModel s) => TaggerEvent -> T.Text -> WidgetNode s TaggerEvent
buttonStdStyle a t =
  button t a
    `styleBasic` [bgColor bgDefault, border 0 bgDefault]
    `styleHover` [bgColor bgLightGray]

tagCommitTagsStringButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
tagCommitTagsStringButton = buttonStdStyle TagCommitTagsString "Tag With"

descriptorCommitNewDescriptorTextButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
descriptorCommitNewDescriptorTextButton =
  buttonStdStyle DescriptorCommitNewDescriptorText "New Descriptor"

shellCmdButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
shellCmdButton = buttonStdStyle ShellCmd "Cmd"

fileSelectionUpdateButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
fileSelectionUpdateButton = flip buttonStdStyle "Select" . FileSelectionUpdate . (: [])

fileSelectionCommitQueryButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
fileSelectionCommitQueryButton = buttonStdStyle FileSelectionCommitQuery "Query"

fileSinglePutButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
fileSinglePutButton = flip buttonStdStyle "Preview" . FileSinglePut

fileSelectionClearButton ::
  (WidgetModel s) =>
  WidgetNode s TaggerEvent
fileSelectionClearButton = buttonStdStyle FileSelectionClear "CS"

requestDescriptorTreeButton ::
  (WidgetModel s) =>
  T.Text ->
  WidgetNode s TaggerEvent
requestDescriptorTreeButton t = buttonStdStyle (RequestDescriptorTree t) "↺"

requestDescriptorTreeButtonForTreeLeaf ::
  WidgetModel s =>
  Descriptor ->
  WidgetNode s TaggerEvent
requestDescriptorTreeButtonForTreeLeaf d =
  buttonStdStyle (RequestDescriptorTree . descriptor $ d) (descriptor d)

refreshUnrelatedDescriptorTreeButton :: (WidgetModel s) => WidgetNode s TaggerEvent
refreshUnrelatedDescriptorTreeButton = buttonStdStyle RefreshUnrelatedDescriptorTree "↺"

descriptorTreePutParentButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
descriptorTreePutParentButton = buttonStdStyle DescriptorTreePutParent "↑"

descriptorDeleteDroppableButton :: WidgetModel s => WidgetNode s TaggerEvent
descriptorDeleteDroppableButton =
  box_ []
    . dropTarget DescriptorDelete
    . buttonStdStyle (PutExtern ())
    $ "X"