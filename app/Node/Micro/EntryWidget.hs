{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Node.Micro.EntryWidget
  ( queryWidget,
    shellCmdWidget,
    newDescriptorWidget,
    tagCommitTagsStringWidget,
  )
where

import qualified Data.Text as T
import Monomer
import Node.Micro.Button
import Node.Micro.TextField
import Type.Model

tagCommitTagsStringWidget ::
  ( WidgetModel s,
    HasTagsString s T.Text
  ) =>
  WidgetNode s TaggerEvent
tagCommitTagsStringWidget =
  keystroke [("Enter", TagCommitTagsString)]
    . hstack_ []
    $ [tagCommitTagsStringButton, tagsStringTextField]

newDescriptorWidget ::
  (WidgetModel s, HasNewDescriptorText s T.Text) =>
  WidgetNode s TaggerEvent
newDescriptorWidget =
  keystroke [("Enter", DescriptorCommitNewDescriptorText)]
    . hstack_ []
    $ [descriptorCommitNewDescriptorTextButton, newDescriptorTextTextField]

shellCmdWidget :: (WidgetModel s, HasShellCmd s T.Text) => WidgetNode s TaggerEvent
shellCmdWidget =
  keystroke [("Enter", ShellCmd)]
    . hstack
    $ [shellCmdTextField, shellCmdButton]

queryWidget ::
  ( WidgetModel s,
    HasFileSelectionQuery s T.Text,
    HasQueryCriteria s QueryCriteria,
    HasFileSetArithmetic s FileSetArithmetic
  ) =>
  WidgetNode s TaggerEvent
queryWidget =
  keystroke [("Enter", FileSelectionCommitQuery)] $
    hgrid_
      []
      [ box_ [] . hstack_ [] $
          [ fileSelectionClearButton,
            fileSelectionCommitQueryButton,
            fileSelectionQueryTextField
          ],
        box_ [] $
          hstack_ [] [setQueryCriteriaDropdown, setArithmeticDropdown]
      ]
  where
    setQueryCriteriaDropdown ::
      (WidgetModel s, WidgetEvent e, HasQueryCriteria s QueryCriteria) =>
      WidgetNode s e
    setQueryCriteriaDropdown =
      dropdown
        queryCriteria
        [ByTag, ByRelation, ByPattern, ByUntagged]
        (label . T.pack . show)
        (label . T.pack . show)
    setArithmeticDropdown ::
      (WidgetModel s, WidgetEvent e, HasFileSetArithmetic s FileSetArithmetic) =>
      WidgetNode s e
    setArithmeticDropdown =
      dropdown
        fileSetArithmetic
        [Union, Intersect, Diff]
        (label . T.pack . show)
        (label . T.pack . show)