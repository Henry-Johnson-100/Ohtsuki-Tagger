{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Node.Base
  ( themeConfig,
    fileWithTagsLabel,
    fileDbWidget,
    fileSinglePreviewWidget,
  )
where

import Control.Lens
import Data.List hiding (concat, intercalate, unlines, unwords)
import Data.Text hiding (map)
import Data.Typeable
import Database.Tagger.Type
import Monomer
import Type.Model
import Prelude hiding (concat, unlines, unwords)

class GetPlainText g where
  getPlainText :: g -> Text

instance GetPlainText File where
  getPlainText = pack . filePath

instance GetPlainText Descriptor where
  getPlainText = pack . descriptor

instance GetPlainText FileWithTags where
  getPlainText = getPlainText . file

themeConfig :: [AppConfig e]
themeConfig =
  [ appWindowTitle "Hello World",
    appTheme lightTheme,
    appFontDef "Regular" "./resources/iosevka_thin.ttf"
  ]

showTags :: [Descriptor] -> [Text]
showTags = map (pack . descriptor) . sort

(!++) :: Text -> Text -> Text
(!++) = Data.Text.append

stdDelayTooltip :: Text -> WidgetNode s e -> WidgetNode s e
stdDelayTooltip = flip tooltip_ [tooltipDelay 750]

fileDbWidget :: (WidgetModel s, WidgetEvent e) => [FileWithTags] -> WidgetNode s e
fileDbWidget fwts =
  let _def = label "bruh"
      fileWithTagsZone =
        map
          ( \(FileWithTags f ts) ->
              let _def = label "bruh"
                  fileSubZone = flip label_ [ellipsis] . getPlainText $ f
                  fwtSpacer = hstack [spacer, label ":", spacer]
                  tagsSubZone = vstack . map (label . getPlainText) $ ts
                  fwtSplit =
                    hstack [fileSubZone, fwtSpacer, tagsSubZone]
               in fwtSplit
          )
      fileWithTagsStack = vstack . fileWithTagsZone $ fwts
   in stdDelayTooltip "File Database" fileWithTagsStack

fileSinglePreviewWidget ::
  ( WidgetModel s,
    WidgetEvent e,
    Type.Model.HasFileSingle s (Maybe FileWithTags)
  ) =>
  s ->
  WidgetNode s e
fileSinglePreviewWidget model =
  maybe (label "") (image . pack . filePath . file) (model ^. fileSingle)

fileWithTagsLabel :: FileWithTags -> WidgetNode s e
fileWithTagsLabel (FileWithTags f ts) =
  let fileLabel = flip label_ [ellipsis] . pack . filePath $ f
      tagStack = vstack (label <$> showTags ts)
      desSpacer = hstack [spacer, label ":", spacer]
   in hstack $ [fileLabel, desSpacer, tagStack]

-- label_ ((pack . filePath) f !++ pack " : " !++ showTags ts) [ellipsis]