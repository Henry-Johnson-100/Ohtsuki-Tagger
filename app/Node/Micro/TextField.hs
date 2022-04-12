{-# LANGUAGE FlexibleContexts #-}

module Node.Micro.TextField
  ( fileSelectionQueryTextField,
    newDescriptorTextTextField,
    tagsStringTextField,
    shellCmdTextField,
  )
where

import qualified Data.Text as T
import Database.Tagger.Type (Descriptor (descriptor))
import Monomer (WidgetModel, WidgetNode, dropTarget, textField_)
import Type.Model
  ( HasFileSelectionQuery (..),
    HasNewDescriptorText (..),
    HasShellCmd (..),
    HasTagsString (..),
    TaggerEvent (FileSelectionAppendQuery, TagsStringAppend),
  )

fileSelectionQueryTextField ::
  (WidgetModel s, HasFileSelectionQuery s T.Text) => WidgetNode s TaggerEvent
fileSelectionQueryTextField =
  dropTarget (FileSelectionAppendQuery . descriptor) $
    textField_ fileSelectionQuery []

newDescriptorTextTextField ::
  (WidgetModel s, HasNewDescriptorText s T.Text) => WidgetNode s TaggerEvent
newDescriptorTextTextField =
  textField_ newDescriptorText []

tagsStringTextField ::
  (WidgetModel s, HasTagsString s T.Text) => WidgetNode s TaggerEvent
tagsStringTextField =
  dropTarget (TagsStringAppend . descriptor) $ textField_ tagsString []

shellCmdTextField ::
  (WidgetModel s, HasShellCmd s T.Text) => WidgetNode s TaggerEvent
shellCmdTextField = textField_ shellCmd []