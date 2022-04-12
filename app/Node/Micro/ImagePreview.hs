{-# LANGUAGE BangPatterns #-}

module Node.Micro.ImagePreview where

import qualified Data.Text as T
import Monomer

-- | Strictly evaluates an image preview widget.
imagePreview ::
  (WidgetModel s, WidgetEvent e) =>
  T.Text ->
  WidgetNode s e
imagePreview fp =
  let !imagePreviewWidget =
        box_ [alignBottom] . flip image_ [alignBottom, fitEither] $ fp
   in imagePreviewWidget