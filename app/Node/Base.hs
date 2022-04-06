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
import Control.Monad
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

bgDefault :: Color
bgDefault = Color 0 0 0 0

bgLightGray :: Color
bgLightGray = Color 203 203 203 0.8

themeConfig :: [AppConfig e]
themeConfig =
  [ appWindowTitle "Hello World",
    appTheme lightTheme,
    appFontDef "Regular" "/usr/local/share/fonts/i/iosevka_light.ttf",
    appFontDef "Thin" "/usr/local/share/fonts/i/iosevka_thin.ttf"
  ]

showTags :: [Descriptor] -> [Text]
showTags = map (pack . descriptor) . sort

(!++) :: Text -> Text -> Text
(!++) = Data.Text.append

stdDelayTooltip :: Text -> WidgetNode s e -> WidgetNode s e
stdDelayTooltip = flip tooltip_ [tooltipDelay 750]

styledButton :: (WidgetModel s) => TaggerEvent -> Text -> WidgetNode s TaggerEvent
styledButton a t =
  button t a
    `styleBasic` [bgColor bgDefault]
    `styleHover` [bgColor bgLightGray]

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
    . flip styleBasic [textFont "Thin"]
    . flip label_ [multiline]
    . fPrintDescriptorTree
  where
    -- #TODO eventually make a button
    -- just don't know what the event will be yet
    dButton :: (WidgetModel s, WidgetEvent e) => Descriptor -> WidgetNode s e
    dButton = label . pack . descriptor
    -- #TODO find a way to format a collection node to be more like a box
    -- Used to group infra nodes to take up less vertical space
    infraBox ::
      (WidgetModel s, WidgetEvent e) =>
      (Descriptor -> WidgetNode s e) ->
      [Descriptor] ->
      WidgetNode s e
    infraBox dsf = box . hstack . map dsf

fileWithTagButton ::
  (WidgetModel s) =>
  TaggerEvent ->
  FileWithTags ->
  WidgetNode s TaggerEvent
fileWithTagButton a fwt =
  case fwt of
    FileWithTags f ts ->
      let fileSubZone = styledButton a (getPlainText f)
          fwtSpacer = hstack [spacer, label ":", spacer]
          tagsSubZone =
            vstack
              ( map
                  ( flip styleBasic [textFont "Thin"]
                      . label
                      . getPlainText
                  )
                  ts
              )
          fwtSplit =
            vstack
              [ hstack
                  [fileSubZone, fwtSpacer, tagsSubZone],
                separatorLine
              ]
              `styleHover` [bgColor bgLightGray]
       in fwtSplit

fileDbWidget :: (WidgetModel s) => [FileWithTags] -> WidgetNode s TaggerEvent
fileDbWidget fwts =
  let fileWithTagsZone =
        map
          (liftM2 fileWithTagButton FileSinglePut id)
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
