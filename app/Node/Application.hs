{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Node.Application
  ( themeConfig,
    fileSelectionWidget,
    fileSinglePreviewWidget,
    descriptorTreeWidget,
    configPanel,
  )
where

import Control.Lens ((^.))
import Data.List (foldl', map)
import Data.Text (Text, append, intercalate, pack, replicate)
import Database.Tagger.Type
import Monomer
import Node.Color
import Node.Micro
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

themeConfig :: [AppConfig e]
themeConfig =
  [ appWindowTitle "Tagger",
    appTheme lightTheme,
    appFontDef "Regular" "/usr/local/share/fonts/i/iosevka_light.ttf",
    appFontDef "Thin" "/usr/local/share/fonts/i/iosevka_thin.ttf",
    appFontDef "Bold" "/usr/local/share/fonts/i/iosevka_bold.ttf"
  ]

(!++) :: Text -> Text -> Text
(!++) = Data.Text.append

configPanel ::
  ( WidgetModel s,
    HasFileSetArithmetic s FileSetArithmetic,
    HasQueryCriteria s QueryCriteria,
    HasFileSelectionQuery s Text
  ) =>
  WidgetNode s TaggerEvent
configPanel =
  box . vgrid $
    [ clearSelectionButton,
      setArithmeticDropdown,
      queryTextField,
      commitQueryButton,
      setQueryCriteriaDropdown
    ]

descriptorTreeWidget ::
  (WidgetModel s, HasDescriptorTree s DescriptorTree) => s -> WidgetNode s TaggerEvent
descriptorTreeWidget model =
  box . stdScroll . flip styleBasic [textFont "Regular"] $
    vstack [resetDescriptorTreeButton, buildTreeWidget (model ^. descriptorTree)]
  where
    buildTreeWidget :: (WidgetModel s) => DescriptorTree -> WidgetNode s TaggerEvent
    buildTreeWidget = buildTreeWidgetAccum 0 (vstack [])
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
            Infra d -> vstack [acc, makeDepthWidget textBlack l d]
            Meta d cs ->
              appendVStack
                (vstack [acc, hstack [makeDepthWidget textBlue l d]])
                ( vstack $
                    map
                      ( \c ->
                          case c of
                            Infra d' -> makeDepthWidget textBlack (l + 1) d'
                            Meta d' _ -> makeDepthWidget textBlue (l + 1) d'
                            NullTree ->
                              spacer
                                `styleBasic` [padding 0, border 0 bgDefault]
                      )
                      cs
                )
        makeDepthWidget textColor' l' d' =
          hstack
            [ label (replicate l' "--" !++ "|"),
              dButton d' `styleBasic` [textColor textColor', bgColor bgDefault, border 0 bgDefault] `styleHover` [bgColor bgLightGray],
              appendToQueryButton (pack . descriptor $ d')
            ]
    appendVStack x y = vstack [x, y]
    dButton :: (WidgetModel s) => Descriptor -> WidgetNode s TaggerEvent
    dButton d =
      flip styleBasic [padding 0, bgColor bgDefault, border 0 bgDefault]
        . flip styledButton (pack . descriptor $ d)
        . RequestDescriptorTree
        . pack
        . descriptor
        $ d

fileSelectionWidget :: (WidgetModel s) => [FileWithTags] -> WidgetNode s TaggerEvent
fileSelectionWidget fwts =
  let fileWithTagsZone =
        map
          (\fwt -> fileWithTagWidget [previewButton fwt, selectButton fwt] fwt)
      fileWithTagsStack = stdScroll $ box_ [] . vstack . fileWithTagsZone $ fwts
   in stdDelayTooltip "File Database" fileWithTagsStack
  where
    -- A widget that shows a FileWithTags, an arbitrary number of buttons
    -- and sizes appropriately to the parent container
    fileWithTagWidget ::
      (WidgetModel s) =>
      [WidgetNode s TaggerEvent] ->
      FileWithTags ->
      WidgetNode s TaggerEvent
    fileWithTagWidget bs fwt =
      let _temp x = const . label $ ""
          buttonGridNode bs' =
            box_ [alignLeft] $ hstack_ [] bs'
          fileNode f' = box_ [alignLeft] $ flip label_ [ellipsis] (pack . filePath $ f')
          tagsNode ts' =
            box_ [alignTop, alignLeft] $
              flip
                label_
                []
                (intercalate ", " . map (pack . descriptor) $ ts')
                `styleBasic` [textColor textBlue]
          fwtSplitNode (fn', tn') =
            box_ [alignLeft] $ vsplit_ [] $ (stdScroll fn', stdScroll tn')
       in box_
            [alignLeft]
            $ vstack_
              []
              [ hstack_ [] $
                  [ buttonGridNode bs,
                    fwtSplitNode (fileNode . file $ fwt, tagsNode . tags $ fwt)
                  ],
                separatorLine
              ]

fileSinglePreviewWidget ::
  (WidgetModel s, HasFileSingle s (Maybe FileWithTags), HasDoSoloTag s Bool) =>
  s ->
  WidgetNode s TaggerEvent
fileSinglePreviewWidget = imageZone
  where
    imageZone ::
      (WidgetModel s, HasFileSingle s (Maybe FileWithTags), HasDoSoloTag s Bool) =>
      s ->
      WidgetNode s TaggerEvent
    imageZone model =
      box_ [onClick ToggleDoSoloTag]
        . vsplit_ []
        $ (imagePreview model, doSoloTagCheckBox)
      where
        imagePreview ::
          (WidgetModel s, WidgetEvent e, HasFileSingle s (Maybe FileWithTags)) =>
          s ->
          WidgetNode s e
        imagePreview m' =
          box_
            []
            $ maybe
              (label "No Preview")
              (flip image_ [alignBottom, fitEither] . pack . filePath . file)
              (m' ^. fileSingle)
        doSoloTagCheckBox ::
          (WidgetModel s, HasDoSoloTag s Bool) => WidgetNode s TaggerEvent
        doSoloTagCheckBox =
          box_
            [alignCenter]
            $ labeledCheckbox_
              "Solo Tagging Mode"
              doSoloTag
              [textRight, maxLines 1, ellipsis]
              `styleBasic` [ textFont "Thin",
                             paddingB 5,
                             paddingT 0,
                             border 0 bgDefault,
                             radius 0
                           ]
