{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use list comprehension" #-}

module Node.Application
  ( themeConfig,
    fileSelectionWidget,
    fileSingleWidget,
    descriptorTreeQuadrantWidget,
    configureZone,
    menubar,
    visibility,
    operationWidget,
  )
where

import Control.Lens
import Data.List (foldl', intersperse, map)
import Data.Text (Text, append, intercalate, pack, replicate, unwords)
import Database.Tagger.Type
import Monomer
import Monomer.Core.Themes.BaseTheme
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
    appTheme yuiTheme,
    appFontDef "Regular" "/usr/local/share/fonts/i/iosevka_light.ttf",
    appFontDef "Thin" "/usr/local/share/fonts/i/iosevka_thin.ttf",
    appFontDef "Bold" "/usr/local/share/fonts/i/iosevka_bold.ttf"
  ]

yuiTheme :: Theme
yuiTheme =
  baseTheme
    lightThemeColors
      { clearColor = yuiLightPeach,
        sectionColor = yuiRed,
        -- btn
        btnBgBasic = yuiLightPeach,
        btnBgFocus = yuiYellow,
        btnFocusBorder = yuiOrange,
        btnBgHover = yuiPeach,
        btnBgActive = yuiOrange,
        -- input
        inputBgBasic = yuiLightPeach,
        inputBgFocus = yuiYellow,
        inputFocusBorder = yuiOrange,
        -- input selected
        inputSelFocus = yuiOrange,
        inputSelBasic = yuiYellow,
        -- dialog
        dialogBg = yuiLightPeach,
        -- sl and dropdowns
        slMainBg = yuiLightPeach,
        -- sl normal
        slNormalBgHover = yuiYellow,
        slNormalFocusBorder = yuiOrange,
        -- sl selected
        slSelectedBgBasic = yuiPeach,
        slSelectedBgHover = yuiOrange,
        slSelectedFocusBorder = yuiRed
      }

menubar :: (WidgetModel s) => WidgetNode s TaggerEvent
menubar =
  vstack_ [] $
    [ hstack_ []
        . (<$>)
          ( `styleBasic`
              [ textSize 12
              ]
                <> buttonStylingBasic
          )
        $ [toggleConfigModeButton],
      separatorLine
    ]

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
    [ hstack [newFileTextField, newFileTextCommitButton],
      dbPathTextField,
      dbBackupTextField,
      dbAutoConnectCheckBox,
      databaseConnectButton,
      configurationExportButton,
      databaseBackupButton,
      initializeDatabaseButton
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

fileSingleWidget ::
  (WidgetModel s, HasDoSoloTag s Bool) =>
  Bool ->
  Maybe FileWithTags ->
  WidgetNode s TaggerEvent
fileSingleWidget isSoloTagMode mfwt =
  flip styleBasic [maxHeight 10000]
    . box_ [alignTop, alignMiddle]
    . hsplit_ [splitIgnoreChildResize True]
    $ ( imagePreview . fmap getPlainText $ mfwt,
        imageDetails isSoloTagMode . maybe [] tags $ mfwt
      )
  where
    imagePreview ::
      (WidgetModel s, HasDoSoloTag s Bool) =>
      Maybe Text ->
      WidgetNode s TaggerEvent
    imagePreview =
      maybe
        (label "No Preview")
        ( box_ [alignMiddle, onClick ToggleDoSoloTag]
            . flip styleBasic [paddingB 3, paddingT 3]
            . flip image_ [fitHeight, alignCenter]
        )
    imageDetails ::
      (WidgetModel s) =>
      Bool ->
      [Descriptor] ->
      WidgetNode s TaggerEvent
    imageDetails isSoloTagMode' ds' =
      flip styleBasic [borderL 1 black, rangeWidth 160 800]
        . box_ [alignLeft]
        . stdScroll
        . vstack_ []
        $ [ label "Details:",
            spacer,
            label "Solo Tagging Mode"
              `styleBasic` [textColor yuiOrange]
              `nodeVisible` isSoloTagMode',
            spacer,
            label "Tags: ",
            hstack_ [] [spacer, vstack_ [] . map imageDetailDescriptor $ ds']
          ]
      where
        imageDetailDescriptor :: (WidgetModel s) => Descriptor -> WidgetNode s TaggerEvent
        imageDetailDescriptor d =
          draggable d
            . flip styleBasic [textColor yuiBlue]
            . label
            . getPlainText
            $ d

operationWidget ::
  ( WidgetModel s,
    HasFileSetArithmetic s FileSetArithmetic,
    HasQueryCriteria s QueryCriteria,
    HasFileSelectionQuery s Text,
    HasTagsString s Text,
    HasTaggingMode s TaggingMode,
    HasNewDescriptorText s Text,
    HasShellCmd s Text
  ) =>
  WidgetNode s TaggerEvent
operationWidget =
  flip styleBasic [border 1 black]
    . box_ [alignTop]
    . flip
      keystroke_
      [ignoreChildrenEvts]
      [ ("Ctrl-y", FileSelectionClear),
        ("Ctrl-u", ShellCmd),
        ("Ctrl-i", DoSingleFileEvent SingleFilePrevFromFileSelection),
        ("Ctrl-k", DoSingleFileEvent SingleFileNextFromFileSelection),
        ("Ctrl-j", FileSetArithmeticNext),
        ("Ctrl-Shift-j", FileSetArithmeticPrev),
        ("Ctrl-l", FileSetQueryCriteriaNext),
        ("Ctrl-Shift-l", FileSetQueryCriteriaPrev),
        ("Ctrl-o", TaggingModeNext),
        ("Ctrl-Shift-o", TaggingModePrev)
      ]
    . vstack_ []
    $ [ selectionOperatorWidget,
        separatorLine,
        labeledQueryTextField,
        separatorLine,
        labeledTagTextField,
        separatorLine,
        labeledNewDescriptorTextField,
        separatorLine,
        labeledShellCmdTextField,
        separatorLine
      ]
  where
    labeledQueryTextField ::
      (WidgetModel s, HasFileSelectionQuery s Text) =>
      WidgetNode s TaggerEvent
    labeledQueryTextField =
      flip
        keystroke_
        [ignoreChildrenEvts]
        [("Enter", FileSelectionCommitQuery)]
        . labeledWidget "Query"
        . hstack_ []
        $ [button "→" FileSelectionCommitQuery, queryTextField]
    labeledTagTextField ::
      (WidgetModel s, HasTagsString s Text) =>
      WidgetNode s TaggerEvent
    labeledTagTextField =
      flip
        keystroke_
        [ignoreChildrenEvts]
        [("Enter", TagCommitTagsString)]
        . labeledWidget "Tag"
        . hstack_ []
        $ [button "→" TagCommitTagsString, tagsStringTextField]
    labeledNewDescriptorTextField ::
      (WidgetModel s, HasNewDescriptorText s Text) =>
      WidgetNode s TaggerEvent
    labeledNewDescriptorTextField =
      flip
        keystroke_
        [ignoreChildrenEvts]
        [("Enter", DescriptorCommitNewDescriptorText)]
        . labeledWidget "Descriptor"
        . hstack_ []
        $ [button "→" DescriptorCommitNewDescriptorText, descriptorNewTextField]
    labeledShellCmdTextField ::
      (WidgetModel s, HasShellCmd s Text) =>
      WidgetNode s TaggerEvent
    labeledShellCmdTextField =
      flip
        keystroke_
        [ignoreChildrenEvts]
        [("Enter", ShellCmd)]
        . labeledWidget "Shell"
        . hstack_ []
        $ [button "→" ShellCmd, shellCmdTextField]
    selectionOperatorWidget ::
      ( WidgetModel s,
        HasFileSetArithmetic s FileSetArithmetic,
        HasQueryCriteria s QueryCriteria,
        HasTaggingMode s TaggingMode
      ) =>
      WidgetNode s TaggerEvent
    selectionOperatorWidget =
      box_ []
        . vgrid_ []
        $ [ hgrid_
              []
              [ hgrid
                  [ stdDelayTooltip "Ctrl-y" clearSelectionButton,
                    stdDelayTooltip "Ctrl-u" doShellCmdButton
                  ],
                stdDelayTooltip "Ctrl-i" fileSinglePrevFromFileSelectionButton,
                stdDelayTooltip "Ctrl-o" taggingModeDropdown
              ],
            hgrid_
              []
              [ stdDelayTooltip "Ctrl-j" setArithmeticDropdown,
                stdDelayTooltip "Ctrl-k" fileSingleNextFromFileSelectionButton,
                stdDelayTooltip "Ctrl-l" setQueryCriteriaDropdown
              ]
          ]

descriptorTreeQuadrantWidget ::
  (WidgetModel s) =>
  DescriptorTree ->
  DescriptorTree ->
  WidgetNode s TaggerEvent
descriptorTreeQuadrantWidget atr utr =
  flip styleBasic [border 1 black] . box_ [alignTop, alignLeft]
    . hsplit_
      [splitIgnoreChildResize True]
    $ (explorableDescriptorTreeWidget atr, unrelatedDescriptorTreeWidget utr)
