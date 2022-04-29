{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Node.Application
  ( themeConfig,
    fileSelectionWidget,
    fileSingleWidget,
    descriptorTreeQuadrantWidget,
    databaseConfigurePage,
    selectionConfigurePage,
    configConfigurationPage,
    descriptorConfigurePage,
    menubar,
    visibility,
    operationWidget,
  )
where

import Control.Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L
import qualified Data.Text as T
import Database.Tagger.Type
import Monomer
import Monomer.Core.Themes.BaseTheme
import Node.Color
import Node.Micro
import Type.Config
import Type.Model

visibility ::
  ( Eq a,
    WidgetModel s,
    HasProgramVisibility s ProgramVisibility,
    HasProgramVisibility s a
  ) =>
  s ->
  a ->
  WidgetNode s e ->
  WidgetNode s e
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

menubar :: WidgetNode TaggerModel TaggerEvent
menubar =
  vstack_ [] $
    [ hstack_ []
        . (<$>)
          ( `styleBasic`
              [ textSize 12
              ]
                <> buttonStylingBasic
          )
        $ [ toggleConfigConfigureVisibility,
            toggleDatabaseConfigureVisibility,
            toggleSelectionConfigureVisibility,
            toggleDescriptorConfigureVisibility,
            -- #TODO move me
            spacer,
            fileSelectionShuffleButton
          ],
      separatorLine
    ]

databaseConfigurePage ::
  WidgetNode TaggerModel TaggerEvent
databaseConfigurePage =
  box . flip styleBasic [padding 80] . vgrid $
    [ hstack [newFileTextField, newFileTextCommitButton],
      dbPathTextField,
      dbBackupTextField,
      dbAutoConnectCheckBox,
      databaseConnectButton,
      databaseBackupButton,
      initializeDatabaseButton
    ]

selectionConfigurePage ::
  (WidgetModel s, HasProgramConfig s TaggerConfig) =>
  WidgetNode s TaggerEvent
selectionConfigurePage =
  box . flip styleBasic [padding 80]
    . vgrid
    $ [selectionDisplayParentsNumberField]

configConfigurationPage :: (WidgetModel s) => WidgetNode s TaggerEvent
configConfigurationPage =
  box . flip styleBasic [padding 80]
    . vgrid
    $ [configurationExportButton]

descriptorConfigurePage :: TaggerModel -> WidgetNode TaggerModel TaggerEvent
descriptorConfigurePage model =
  box . flip styleBasic [padding 80]
    . vstack
    $ [ labeledWidget "Main Tree Root" descriptorTreeConfigureMainRequestTextField,
        spacer,
        renameDescriptorWidget,
        spacer,
        label "Database Meta-Descriptor Hierarchy: ",
        generalDescriptorTreeWidget
          (model ^. (descriptorModel . mainDescriptorTree . rootTree))
          [ resetDescriptorTreeToButton "#ALL#",
            parentDescriptorTreeButton,
            descriptorDeleteWidget
          ]
          treeLeafButtonRequestDescriptorTree
          (model ^. (programConfig . descriptorTreeConf))
      ]

fileSelectionWidget ::
  (WidgetModel s) =>
  Int ->
  [FileWithTags] ->
  WidgetNode s TaggerEvent
fileSelectionWidget dispParents fwts =
  let fileWithTagsZone =
        map
          ( \fwt ->
              fileWithTagWidget
                [previewButton fwt, selectButton fwt]
                dispParents
                fwt
          )
      fileWithTagsStack = stdScroll $ box_ [] . vstack . fileWithTagsZone $ fwts
   in stdDelayTooltip "File Database" fileWithTagsStack
  where
    -- A widget that shows a FileWithTags, an arbitrary number of buttons
    -- and sizes appropriately to the parent container
    fileWithTagWidget ::
      (WidgetModel s) =>
      [WidgetNode s TaggerEvent] ->
      Int ->
      FileWithTags ->
      WidgetNode s TaggerEvent
    fileWithTagWidget bs dispParents' fwt =
      let buttonGridNode bs' =
            box_ [alignLeft] $ hstack_ [] bs'
          fileNode f' =
            box_ [alignLeft] . flip label_ [ellipsis] . getPathComponents dispParents' $
              (filePath $ f')
          tagsNode ts' = draggableDescriptorListWidget ts'
          fwtSplitNode (fn', tn') =
            box_ [alignLeft] $ vsplit_ [] $ (stdScroll fn', stdScroll tn')
       in box_
            [alignLeft]
            $ vstack_
              []
              [ hstack_ [] $
                  [ buttonGridNode bs,
                    box_ [alignLeft] . fileNode . file $ fwt
                  ],
                separatorLine
              ]
      where
        getPathComponents :: Int -> T.Text -> T.Text
        getPathComponents n p =
          let !brokenPath = T.splitOn "/" p
              !droppedDirs = length brokenPath - n
           in (!++) ((T.pack . show) droppedDirs !++ ".../")
                . T.intercalate "/"
                . drop droppedDirs
                $ brokenPath

fileSingleWidget ::
  (WidgetModel s, HasDoSoloTag s Bool) =>
  Bool ->
  [FileWithTags] ->
  SingleFileSelectionModel ->
  WidgetNode s TaggerEvent
fileSingleWidget isSoloTagMode currentFileSelection sfModel =
  flip styleBasic [maxHeight 10000]
    . box_ [alignTop, alignMiddle]
    . hsplit_ [splitIgnoreChildResize True]
    $ ( imagePreview . fmap getPlainText $ sfModel ^. singleFile,
        imageDetailWidget isSoloTagMode currentFileSelection (sfModel ^. tagCounts)
      )
  where
    imagePreview ::
      (WidgetModel s, HasDoSoloTag s Bool) =>
      Maybe T.Text ->
      WidgetNode s TaggerEvent
    imagePreview =
      maybe
        (label "No Preview")
        ( box_ [alignMiddle, onClick ToggleDoSoloTag]
            . flip styleBasic [paddingB 3, paddingT 3]
            . flip image_ [fitHeight, alignCenter]
        )

operationWidget ::
  WidgetNode TaggerModel TaggerEvent
operationWidget =
  flip styleBasic [border 1 black]
    . box_ [alignTop]
    . flip
      keystroke_
      [ignoreChildrenEvts]
      [ ("Ctrl-y", DoFileSelectionEvent FileSelectionClear),
        ("Ctrl-u", ShellCmd),
        ("Ctrl-i", DoSingleFileEvent SingleFilePrevFromFileSelection),
        ("Ctrl-k", DoSingleFileEvent SingleFileNextFromFileSelection),
        ("Ctrl-j", DoFileSelectionEvent FileSelectionNextSetArithmetic),
        ("Ctrl-Shift-j", DoFileSelectionEvent FileSelectionPrevSetArithmetic),
        ("Ctrl-l", DoFileSelectionEvent FileSelectionNextQueryCriteria),
        ("Ctrl-Shift-l", DoFileSelectionEvent FileSelectionPrevQueryCriteria),
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
      WidgetNode TaggerModel TaggerEvent
    labeledQueryTextField =
      flip
        keystroke_
        [ignoreChildrenEvts]
        [("Enter", DoFileSelectionEvent FileSelectionCommitQueryText)]
        . labeledWidget "Query"
        . hstack_ []
        $ [ button "→" (DoFileSelectionEvent FileSelectionCommitQueryText),
            queryTextField
          ]
    labeledTagTextField ::
      WidgetNode TaggerModel TaggerEvent
    labeledTagTextField =
      flip
        keystroke_
        [ignoreChildrenEvts]
        [("Enter", TagCommitTagsString)]
        . labeledWidget "Tag"
        . hstack_ []
        $ [button "→" TagCommitTagsString, tagsStringTextField]
    labeledNewDescriptorTextField ::
      ( WidgetModel s,
        HasNewDescriptorText s T.Text
      ) =>
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
      ( WidgetModel s,
        HasShellCmd s T.Text
      ) =>
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
      -- ( WidgetModel s,
      --   HasTaggingMode s TaggingMode,
      --   HasSetArithmetic s FileSetArithmetic,
      --   HasFileSelectionModel s FileSelectionModel,
      --   HasQueryCriteria s QueryCriteria
      -- ) =>
      WidgetNode TaggerModel TaggerEvent
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
  DescriptorTreeConfig ->
  DescriptorTree ->
  DescriptorTree ->
  WidgetNode TaggerModel TaggerEvent
descriptorTreeQuadrantWidget dtrConf atr utr =
  flip styleBasic [border 1 black] . box_ [alignTop, alignLeft]
    . hsplit_
      [splitIgnoreChildResize True]
    $ (mainDescriptorTreeWidget dtrConf atr, unrelatedDescriptorTreeWidget dtrConf utr)
