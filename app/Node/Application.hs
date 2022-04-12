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
    descriptorTreeQuadrantWidget,
    configPanel,
    queryWidget,
  )
where

import Control.Lens ((^.))
import Data.List (foldl', intersperse, map)
import Data.Text (Text, append, intercalate, pack, replicate, unwords)
import Database.Tagger.Type
import Monomer
import Node.Micro
import Node.Micro.Button
import Node.Micro.Colors
import Node.Micro.TextField
import Type.Model
import Prelude hiding (concat, replicate, unlines, unwords)

themeConfig :: [AppConfig e]
themeConfig =
  [ appWindowTitle "Tagger",
    appTheme lightTheme,
    appFontDef "Regular" "/usr/local/share/fonts/i/iosevka_light.ttf",
    appFontDef "Thin" "/usr/local/share/fonts/i/iosevka_thin.ttf",
    appFontDef "Bold" "/usr/local/share/fonts/i/iosevka_bold.ttf"
  ]

configPanel ::
  ( WidgetModel s,
    HasFileSetArithmetic s FileSetArithmetic,
    HasQueryCriteria s QueryCriteria,
    HasFileSelectionQuery s Text,
    HasShellCmd s Text,
    HasTagsString s Text,
    HasNewDescriptorText s Text
  ) =>
  WidgetNode s TaggerEvent
configPanel =
  box . vgrid $
    [ shellCmdWidget,
      tagCommitTagsStringWidget,
      newDescriptorWidget
    ]

fileSelectionWidget :: (WidgetModel s) => [FileWithTags] -> WidgetNode s TaggerEvent
fileSelectionWidget fwts =
  let fileWithTagsZone =
        map
          (\fwt -> fileWithTagWidget [fileSinglePutButton fwt, fileSelectionUpdateButton fwt] fwt)
      fileWithTagsStack = stdScroll $ box_ [] . vstack . fileWithTagsZone $ fwts
   in fileWithTagsStack
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
          fileNode f' = box_ [alignLeft] $ flip label_ [ellipsis] (filePath $ f')
          tagsNode ts' = draggableDescriptorListWidget ts'
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
        $ (imagePreview model, vstack [singleFileTags, doSoloTagCheckBox])
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
              (Node.Micro.imagePreview . filePath . file)
              (m' ^. fileSingle)
        singleFileTags ::
          ( WidgetModel s,
            WidgetEvent e,
            HasFileSingle s (Maybe FileWithTags)
          ) =>
          WidgetNode s e
        singleFileTags =
          maybe
            spacer
            ( draggableDescriptorListWidget
                . tags
            )
            (model ^. fileSingle)
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

descriptorTreeQuadrantWidget :: (WidgetModel s) => DescriptorTree -> DescriptorTree -> WidgetNode s TaggerEvent
descriptorTreeQuadrantWidget atr utr =
  flip styleBasic [border 1 textBlack] . box_ [alignTop, alignLeft] $
    hsplit (explorableDescriptorTreeWidget atr, unrelatedDescriptorTreeWidget utr)
