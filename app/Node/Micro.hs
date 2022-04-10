{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use lambda-case" #-}

module Node.Micro where

import Data.List
import Data.Text hiding (foldl', intersperse, map)
import Database.Tagger.Type
import Monomer
import Node.Color
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
queryTextField =
  dropTarget (FileSelectionAppendQuery . descriptor) $
    textField_ fileSelectionQuery []

tagsStringTextField ::
  (WidgetModel s, HasTagsString s Text) => WidgetNode s TaggerEvent
tagsStringTextField =
  dropTarget (TagsStringAppend . descriptor) $ textField_ tagsString []

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

parentDescriptorTreeButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
parentDescriptorTreeButton = styledButton DescriptorTreePutParent "Up"

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
    . flip styleBasic [textColor textBlue]
    . flip label_ [ellipsis]
    . getPlainText
    $ d

treeLeafDescriptorWidget ::
  WidgetModel s =>
  Color ->
  Int ->
  Descriptor ->
  WidgetNode s TaggerEvent
treeLeafDescriptorWidget tc l d =
  hstack_ [] $
    [ label (Data.Text.replicate l "--" !++ "|"),
      draggable d $
        styledButton (RequestDescriptorTree . descriptor $ d) (descriptor d)
          `styleBasic` [ textColor tc,
                         bgColor bgDefault,
                         border 0 bgDefault,
                         padding 0
                       ]
          `styleHover` [bgColor bgLightGray]
    ]

descriptorTreeWidget ::
  (WidgetModel s) => DescriptorTree -> WidgetNode s TaggerEvent
descriptorTreeWidget tr =
  box . stdScroll . flip styleBasic [textFont "Regular"] . buildTreeWidget $ tr
  where
    buildTreeWidget :: (WidgetModel s) => DescriptorTree -> WidgetNode s TaggerEvent
    buildTreeWidget = buildTreeWidgetAccum 0 (vstack [])
      where
        buildTreeWidgetAccum ::
          (WidgetModel s) =>
          Int ->
          WidgetNode s TaggerEvent ->
          DescriptorTree ->
          WidgetNode s TaggerEvent
        buildTreeWidgetAccum l acc tr =
          case tr of
            NullTree -> acc
            Infra d -> vstack [acc, treeLeafDescriptorWidget textBlack l d]
            Meta d cs ->
              appendVStack
                (vstack [acc, hstack [treeLeafDescriptorWidget textBlue l d]])
                ( vstack $
                    map
                      ( \c ->
                          case c of
                            Infra d' -> treeLeafDescriptorWidget textBlack (l + 1) d'
                            Meta d' _ -> treeLeafDescriptorWidget textBlue (l + 1) d'
                            NullTree ->
                              spacer
                                `styleBasic` [padding 0, border 0 bgDefault]
                      )
                      cs
                )
    appendVStack x y = vstack [x, y]