{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Eta reduce" #-}

module Node.Base
  ( themeConfig,
    fileDbWidget,
    fileSinglePreviewWidget,
    descriptorTreeWidget,
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

previewImageButton :: (WidgetModel s) => FileWithTags -> WidgetNode s TaggerEvent
previewImageButton = button "Preview" . FileSinglePut

fPrintDescriptorTree :: DescriptorTree -> Text
fPrintDescriptorTree = p "" 0
  where
    p :: Text -> Int -> DescriptorTree -> Text
    p t l tr =
      case tr of
        NullTree -> t
        -- Infra (Descriptor d) -> append t (append (Data.Text.replicate l "-") (pack d))
        Infra (Descriptor d) ->
          t
            !++ "\n"
            !++ Data.Text.replicate l " "
            !++ "|"
            !++ pack d
        Meta (Descriptor d) cs ->
          t !++ "\n"
            !++ Data.List.foldl'
              (\t' tr' -> p t' (l + 1) tr')
              (Data.Text.replicate l " " !++ "|" !++ pack d)
              cs

descriptorTreeWidget ::
  (WidgetModel s, WidgetEvent e) => DescriptorTree -> WidgetNode s e
descriptorTreeWidget =
  box
    . scroll_ [wheelRate 50]
    . flip label_ [multiline]
    . fPrintDescriptorTree

fileDbWidget :: (WidgetModel s) => [FileWithTags] -> WidgetNode s TaggerEvent
fileDbWidget fwts =
  let fileWithTagsZone =
        map
          ( \fwt ->
              case fwt of
                (FileWithTags f ts) ->
                  let fileSubZone = flip label_ [ellipsis] . getPlainText $ f
                      fwtSpacer = hstack [spacer, label ":", spacer]
                      tagsSubZone = vstack . map (label . getPlainText) $ ts
                      fileDbSinglePutButton = previewImageButton fwt
                      fwtSplit =
                        hstack
                          [ fileSubZone,
                            fwtSpacer,
                            tagsSubZone,
                            fileDbSinglePutButton
                          ]
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
  let imagePreview =
        maybe
          (label "No Preview")
          (flip image_ [alignMiddle, fitEither] . pack . filePath . file)
          (model ^. fileSingle)
      boxedPreview = stdDelayTooltip "Image Preivew" . box $ imagePreview
   in boxedPreview
