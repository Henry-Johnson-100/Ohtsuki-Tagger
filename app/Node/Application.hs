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
    explorableDescriptorTreeWidget,
    configPanel,
    queryWidget,
  )
where

import Control.Lens ((^.))
import Data.List (foldl', intersperse, map)
import Data.Text (Text, append, intercalate, pack, replicate, unwords)
import Database.Tagger.Type
import Monomer
import Node.Color
import Node.Micro
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
    HasTagsString s Text
  ) =>
  WidgetNode s TaggerEvent
configPanel =
  box . vgrid $
    [ shellCmdWidget,
      tagCommitWidget
    ]

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
              (flip image_ [alignBottom, fitEither] . getPlainText)
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

queryWidget ::
  ( WidgetModel s,
    HasFileSelectionQuery s Text,
    HasQueryCriteria s QueryCriteria,
    HasFileSetArithmetic s FileSetArithmetic
  ) =>
  WidgetNode s TaggerEvent
queryWidget =
  keystroke [("Enter", FileSelectionCommitQuery)] $
    hgrid_
      []
      [ box_ [] . hstack_ [] $ [clearSelectionButton, commitQueryButton, queryTextField],
        box_ [] $
          hstack_ [] [setQueryCriteriaDropdown, setArithmeticDropdown]
      ]

tagCommitWidget ::
  ( WidgetModel s,
    HasTagsString s Text
  ) =>
  WidgetNode s TaggerEvent
tagCommitWidget =
  keystroke [("Enter", TagCommitTagsString)]
    . hstack_ []
    $ [tagCommitButton, tagsStringTextField]

explorableDescriptorTreeWidget ::
  (WidgetModel s) => DescriptorTree -> WidgetNode s TaggerEvent
explorableDescriptorTreeWidget tr =
  flip styleBasic [border 1 textBlack]
    . box_ [alignTop, alignLeft]
    $ hstack_
      []
      [ vsplit
          ( vstack_
              []
              [ resetDescriptorTreeButton,
                parentDescriptorTreeButton
              ],
            spacer
          ),
        separatorLine,
        descriptorTreeWidget tr
      ]
