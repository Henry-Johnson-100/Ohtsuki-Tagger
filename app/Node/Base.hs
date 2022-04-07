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
    configPanel,
  )
where

import Control.Lens
import Control.Monad
import Data.List hiding (concat, intercalate, replicate, unlines, unwords)
import Data.Text hiding (map)
import Data.Typeable
import Database.Tagger.Type
import Monomer
import Type.Model
import Prelude hiding (concat, replicate, unlines, unwords)

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
    `styleBasic` [bgColor bgDefault, border 0 bgDefault]
    `styleHover` [bgColor bgLightGray]

configPanel ::
  (WidgetModel s, WidgetEvent e, HasFileSetArithmetic s FileSetArithmetic) =>
  WidgetNode s e
configPanel = box . vgrid $ [setArithmeticDropDown]

setArithmeticDropDown ::
  (WidgetModel s, WidgetEvent e, HasFileSetArithmetic s FileSetArithmetic) =>
  WidgetNode s e
setArithmeticDropDown =
  dropdown
    fileSetArithmetic
    [Union, Intersect, Diff]
    (label . pack . show)
    (label . pack . show)

resetDescriptorTreeButton ::
  (WidgetModel s) =>
  WidgetNode s TaggerEvent
resetDescriptorTreeButton = styledButton (RequestDescriptorTree "META") "Top"

selectButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
selectButton = flip styledButton "Select" . FileSelectionUpdate . (: [])

previewButton ::
  (WidgetModel s) =>
  FileWithTags ->
  WidgetNode s TaggerEvent
previewButton = flip styledButton "Preview" . FileSinglePut

descriptorTreeWidget ::
  (WidgetModel s, HasDescriptorTree s DescriptorTree) => s -> WidgetNode s TaggerEvent
descriptorTreeWidget model =
  box . scroll_ [wheelRate 50] . flip styleBasic [textFont "Regular"] $
    vstack [resetDescriptorTreeButton, buildTreeWidget (model ^. descriptorTree)]
  where
    buildTreeWidget :: (WidgetModel s) => DescriptorTree -> WidgetNode s TaggerEvent
    buildTreeWidget = buildTreeWidgetAccum 1 (vstack [])
      where
        buildTreeWidgetAccum ::
          (WidgetModel s) =>
          Int ->
          WidgetNode s TaggerEvent ->
          DescriptorTree ->
          WidgetNode s TaggerEvent
        buildTreeWidgetAccum l acc tr =
          case tr of
            NullTree -> acc
            Infra d -> vstack [acc, makeDepthWidget l d]
            Meta d cs ->
              Data.List.foldl'
                (buildTreeWidgetAccum (l + 1))
                (vstack [acc, hstack [makeDepthWidget l d]])
                cs
        makeDepthWidget l' d' = hstack [label (replicate l' " " !++ "|"), dButton d']
    appendVStack x y = vstack [x, y]
    dButton :: (WidgetModel s) => Descriptor -> WidgetNode s TaggerEvent
    dButton d =
      flip styleBasic [padding 0, bgColor bgDefault]
        . flip styledButton (pack . descriptor $ d)
        . RequestDescriptorTree
        . pack
        . descriptor
        $ d

fileWithTagButton ::
  (WidgetModel s) =>
  TaggerEvent ->
  FileWithTags ->
  WidgetNode s TaggerEvent
fileWithTagButton a fwt =
  case fwt of
    FileWithTags f ts ->
      let fileSubZone = styledButton a (getPlainText f)
          fwtSpacer =
            label ":" `styleBasic` [paddingL 1, paddingR 1, paddingT 0, paddingB 0]
          tagsSubZone =
            scroll . flip label_ [multiline] . unlines . map getPlainText $ ts
          -- hgrid
          --   ( map
          --       ( flip styleBasic [textFont "Thin"]
          --           . label
          --           . getPlainText
          --       )
          --       ts
          --   )
          fwtSplit =
            vstack
              [ hstack
                  [fileSubZone, fwtSpacer, tagsSubZone],
                separatorLine
              ]
              `styleHover` [bgColor bgLightGray]
       in box fwtSplit

fileDbWidget :: (WidgetModel s) => [FileWithTags] -> WidgetNode s TaggerEvent
fileDbWidget fwts =
  let fileWithTagsZone =
        map
          (liftM2 fileWithTagButton FileSinglePut id)
      fileWithTagsStack = box . vstack . fileWithTagsZone $ fwts
   in stdDelayTooltip "File Database" fileWithTagsStack

fileSinglePreviewWidget ::
  ( HasDoSoloTag s1 Bool,
    HasFileSingle s2 (Maybe FileWithTags),
    Typeable s1
  ) =>
  s2 ->
  WidgetNode s1 TaggerEvent
fileSinglePreviewWidget model =
  let imagePreview =
        box_ [] $
          maybe
            (label "No Preview")
            (flip image_ [alignBottom, fitEither] . pack . filePath . file)
            (model ^. fileSingle)
      doSoloTagCheckbox =
        box_ [alignCenter] $
          labeledCheckbox_
            "Solo Tagging Mode"
            doSoloTag
            [textRight, maxLines 1, ellipsis]
            `styleBasic` [ textFont "Thin",
                           paddingB 5,
                           paddingT 0,
                           border 0 bgDefault,
                           radius 0
                         ]
      imageZone =
        box_ [onClick ToggleDoSoloTag]
          . vsplit_ []
          $ (imagePreview, doSoloTagCheckbox)
   in imageZone
