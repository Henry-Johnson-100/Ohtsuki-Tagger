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

draggableDescriptorListWidget ::
  (WidgetModel s, WidgetEvent e) => [Descriptor] -> WidgetNode s e
draggableDescriptorListWidget =
  box_ [alignLeft]
    . hstack
    . intersperse spacer
    . foldl' (\ws d -> ws ++ [draggableDescriptorWidget d]) []
  where
    draggableDescriptorWidget ::
      (WidgetModel s, WidgetEvent e) => Descriptor -> WidgetNode s e
    draggableDescriptorWidget d =
      box_ [alignLeft]
        . draggable d
        . flip styleBasic [textColor textBlue]
        . flip label_ [ellipsis]
        . getPlainText
        $ d

