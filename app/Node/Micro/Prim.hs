{-# LANGUAGE OverloadedStrings #-}

module Node.Micro.Prim
  ( module Node.Micro.Prim,
  )
where

import qualified Data.Text as T
import Database.Tagger.Type
import Monomer
import Node.Micro.Colors
import Type.Model

(!++) :: T.Text -> T.Text -> T.Text
(!++) = T.append

class GetPlainText g where
  getPlainText :: g -> T.Text

instance GetPlainText File where
  getPlainText = filePath

instance GetPlainText Descriptor where
  getPlainText = descriptor

instance GetPlainText FileWithTags where
  getPlainText = getPlainText . file

styledButton :: (WidgetModel s) => TaggerEvent -> T.Text -> WidgetNode s TaggerEvent
styledButton a t =
  button t a
    `styleBasic` [bgColor bgDefault, border 0 bgDefault]
    `styleHover` [bgColor bgLightGray]

-- #TODO move to Node.Micro.Prim
stdScroll :: WidgetNode s e -> WidgetNode s e
stdScroll = scroll_ [wheelRate 50]

descriptorDeleteWidget :: WidgetModel s => WidgetNode s TaggerEvent
descriptorDeleteWidget =
  box_ []
    . dropTarget DescriptorDelete
    . styledButton (PutExtern ())
    $ "X"