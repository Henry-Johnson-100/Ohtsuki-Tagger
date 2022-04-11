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

descriptorNewTextField ::
  (WidgetModel s, HasNewDescriptorText s Text) => WidgetNode s TaggerEvent
descriptorNewTextField =
  textField_ newDescriptorText []

tagsStringTextField ::
  (WidgetModel s, HasTagsString s Text) => WidgetNode s TaggerEvent
tagsStringTextField =
  dropTarget (TagsStringAppend . descriptor) $ textField_ tagsString []

tagCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
tagCommitButton = styledButton TagCommitTagsString "Tag With"

descriptorNewCommitButton ::
  (WidgetModel s) => WidgetNode s TaggerEvent
descriptorNewCommitButton = styledButton DescriptorCommitNewDescriptorText "New Descriptor"

descriptorDeleteWidget :: WidgetModel s => WidgetNode s TaggerEvent
descriptorDeleteWidget = box_ [] . dropTarget DescriptorDelete . flip label_ [] $ "Delete Descriptor"

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
previewButton = flip styledButton "Preview" . FileSinglePut

clearSelectionButton ::
  (WidgetModel s) =>
  WidgetNode s TaggerEvent
clearSelectionButton = styledButton FileSelectionClear "CS"

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
    . flip styleBasic [textColor textBlue]
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
  flip styleBasic [border 1 textBlack] . box_ [alignTop, alignLeft] $
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
          (PutExtern ())
          (\m' -> DescriptorCreateRelation [m'] [d'])
          (getNode tr)
    )
    $ generalDescriptorTreeWidget
      tr
      [resetDescriptorTreeToButton "#ALL#", parentDescriptorTreeButton]
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
                         bgColor bgDefault,
                         border 0 bgDefault,
                         padding 0
                       ]
          `styleHover` [bgColor bgLightGray]
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
                    textBlack
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
                            textBlue
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
                                textBlack
                                (l + 1)
                                d'
                                action
                            Meta d' _ ->
                              treeLeafDescriptorWidget
                                textBlue
                                (l + 1)
                                d'
                                action
                            NullTree ->
                              spacer
                                `styleBasic` [padding 0, border 0 bgDefault]
                      )
                      cs
                )
    appendVStack x y = vstack [x, y]