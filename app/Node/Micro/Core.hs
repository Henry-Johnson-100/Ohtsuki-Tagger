{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use lambda-case" #-}

module Node.Micro.Core
  ( module Node.Micro.Core,
    module Node.Micro.DescriptorTree,
    stdScroll,
  )
where

import Data.List
import Data.Text hiding (foldl', intersperse, map)
import Database.Tagger.Type
import Monomer
import Node.Micro.Button
import Node.Micro.Colors
import Node.Micro.DescriptorTree
import Node.Micro.Prim
import Node.Micro.TextField
import Type.Model

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

shellCmdWidget :: (WidgetModel s, HasShellCmd s Text) => WidgetNode s TaggerEvent
shellCmdWidget =
  keystroke [("Enter", ShellCmd)]
    . hstack
    $ [shellCmdTextField, shellCmdButton]

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

-- | Strictly evaluates an image preview widget.
imagePreview ::
  (WidgetModel s, WidgetEvent e) =>
  Text ->
  WidgetNode s e
imagePreview fp =
  let !imagePreviewWidget =
        box_ [alignBottom] . flip image_ [alignBottom, fitEither] $ fp
   in imagePreviewWidget