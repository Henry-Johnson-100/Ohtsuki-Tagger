{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Interface.Widget.Internal.FilePreview (
  widget,
) where

import Control.Lens ((^.))
import Data.Event (
  FocusedFileEvent (PutFile, TagFile, UnSubTag),
  TaggerEvent (DoFocusedFileEvent),
 )
import Data.Model (
  HasFocusedFile (focusedFile),
  HasFocusedFileModel (focusedFileModel),
  TaggerModel,
 )
import Database.Tagger (
  ConcreteTag (concreteTagId),
  ConcreteTaggedFile (concreteTaggedFile),
  Descriptor (Descriptor),
  File (filePath),
 )
import Interface.Theme (yuiBlue, yuiOrange, yuiRed)
import Interface.Widget.Internal.Core (withStyleBasic)
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  CmbAlignCenter (alignCenter),
  CmbBorder (border),
  CmbFitEither (fitEither),
  CmbMergeRequired (mergeRequired),
  box_,
  dropTargetStyle,
  dropTarget_,
  image_,
 )

widget :: TaggerModel -> TaggerWidget
widget
  ( filePath
      . concreteTaggedFile
      . (^. focusedFileModel . focusedFile) ->
      fp
    ) =
    dropTargets
      . styles
      . box_
        [mergeRequired mergeRequirement]
      $ image_ fp [fitEither, alignCenter]

styles :: TaggerWidget -> TaggerWidget
styles = withStyleBasic []

dropTargets :: TaggerWidget -> TaggerWidget
dropTargets =
  dropTarget_
    (DoFocusedFileEvent . PutFile)
    [dropTargetStyle [border 3 yuiOrange]]
    . dropTarget_
      (\(Descriptor dk _) -> DoFocusedFileEvent (TagFile dk Nothing))
      [dropTargetStyle [border 3 yuiBlue]]
    . dropTarget_
      (DoFocusedFileEvent . UnSubTag . concreteTagId)
      [dropTargetStyle [border 3 yuiRed]]

{- |
 If the file changes or the concrete tags attached to the file change.
-}
mergeRequirement ::
  p ->
  TaggerModel ->
  TaggerModel ->
  Bool
mergeRequirement _ m1 m2 =
  concreteTaggedFile (m1 ^. focusedFileModel . focusedFile)
    /= concreteTaggedFile (m2 ^. focusedFileModel . focusedFile)