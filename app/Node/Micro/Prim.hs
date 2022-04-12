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

stdScroll :: WidgetNode s e -> WidgetNode s e
stdScroll = scroll_ [wheelRate 50]
