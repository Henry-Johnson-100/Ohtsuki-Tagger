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
    configureZone,
    queryAndTagEntryWidget,
    menubar,
    visibility,
  )
where

import Control.Lens ((^.))
import Data.List (foldl', intersperse, map)
import Data.Text (Text, append, intercalate, pack, replicate, unwords)
import Database.Tagger.Type
import Monomer
import Node.Color
import Node.Micro
import Type.Config
import Type.Model
import Prelude hiding (concat, replicate, unlines, unwords)

visibility ::
  (Eq a, HasProgramVisibility s1 a) =>
  s1 ->
  a ->
  WidgetNode s2 e ->
  WidgetNode s2 e
visibility m vm = flip nodeVisible (vm == m ^. programVisibility)

themeConfig :: [AppConfig e]
themeConfig =
  [ appWindowTitle "Tagger",
    appTheme lightTheme,
    appFontDef "Regular" "/usr/local/share/fonts/i/iosevka_light.ttf",
    appFontDef "Thin" "/usr/local/share/fonts/i/iosevka_thin.ttf",
    appFontDef "Bold" "/usr/local/share/fonts/i/iosevka_bold.ttf"
  ]

menubar :: (WidgetModel s) => WidgetNode s TaggerEvent
menubar =
  box_ [alignLeft] . hstack_ []
    . (<$>)
      ( `styleBasic`
          [ textSize 12
          ]
            <> buttonStylingBasic
      )
    $ [toggleConfigModeButton]

configureZone ::
  ( WidgetModel s,
    HasFileSetArithmetic s FileSetArithmetic,
    HasQueryCriteria s QueryCriteria,
    HasFileSelectionQuery s Text,
    HasShellCmd s Text,
    HasTagsString s Text,
    HasNewDescriptorText s Text,
    HasTaggingMode s TaggingMode,
    HasNewFileText s Text,
    HasProgramConfig s TaggerConfig
  ) =>
  WidgetNode s TaggerEvent
configureZone =
  box . vgrid $
    [ shellCmdWidget,
      descriptorNewWidget,
      hstack [newFileTextField, newFileTextCommitButton],
      dbPathTextField,
      dbBackupTextField,
      dbAutoConnectCheckBox,
      databaseConnectButton,
      configurationExportButton
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
      box_ [onClick ToggleDoSoloTag, alignMiddle]
        . vsplit_ []
        $ (imagePreview model, vstack [singleFileTags, doSoloTagCheckBox])
      where
        imagePreview ::
          (WidgetModel s, WidgetEvent e, HasFileSingle s (Maybe FileWithTags)) =>
          s ->
          WidgetNode s e
        imagePreview m' =
          box_
            [alignMiddle]
            $ maybe
              (label "No Preview")
              (flip image_ [alignMiddle, fitEither] . getPlainText)
              (m' ^. fileSingle)
        singleFileTags ::
          ( WidgetModel s,
            WidgetEvent e,
            HasFileSingle s (Maybe FileWithTags)
          ) =>
          WidgetNode s e
        singleFileTags =
          box_ [] $
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
  keystroke_
    [ ("Ctrl-c", FileSetQueryCriteriaNext),
      ("Ctrl-s", FileSetArithmeticNext)
    ]
    [ignoreChildrenEvts]
    . box_ []
    . hstack_ []
    $ [ clearSelectionButton,
        setArithmeticDropdown,
        commitQueryButton,
        keystroke [("Enter", FileSelectionCommitQuery)] queryTextField,
        setQueryCriteriaDropdown
      ]

tagCommitWidget ::
  ( WidgetModel s,
    HasTagsString s Text,
    HasTaggingMode s TaggingMode
  ) =>
  WidgetNode s TaggerEvent
tagCommitWidget =
  box_ [alignLeft]
    . hstack_ []
    $ [ taggingModeDropdown,
        keystroke
          [ ("Enter", TagCommitTagsString),
            ("Ctrl-j", FileSinglePrevFromFileSelection),
            ("Ctrl-k", FileSingleNextFromFileSelection)
          ]
          . hstack_ []
          $ [ tagCommitButton,
              stdDelayTooltip "Ctrl-j" fileSinglePrevFromFileSelectionButton,
              stdDelayTooltip "Ctrl-k" fileSingleNextFromFileSelectionButton,
              tagsStringTextField
            ]
      ]

queryAndTagEntryWidget ::
  ( WidgetModel s,
    HasTagsString s Text,
    HasTaggingMode s TaggingMode,
    HasFileSelectionQuery s Text,
    HasQueryCriteria s QueryCriteria,
    HasFileSetArithmetic s FileSetArithmetic
  ) =>
  WidgetNode s TaggerEvent
queryAndTagEntryWidget = vstack [queryWidget, tagCommitWidget]

descriptorNewWidget :: (WidgetModel s, HasNewDescriptorText s Text) => WidgetNode s TaggerEvent
descriptorNewWidget = keystroke [("Enter", DescriptorCommitNewDescriptorText)] . hstack_ [] $ [descriptorNewCommitButton, descriptorNewTextField]

descriptorTreeQuadrantWidget :: (WidgetModel s) => DescriptorTree -> DescriptorTree -> WidgetNode s TaggerEvent
descriptorTreeQuadrantWidget atr utr =
  flip styleBasic [border 1 textBlack] . box_ [alignTop, alignLeft] $
    hsplit (explorableDescriptorTreeWidget atr, unrelatedDescriptorTreeWidget utr)
