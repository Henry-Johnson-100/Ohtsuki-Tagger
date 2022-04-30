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
import Type.BufferList
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
            toggleDescriptorConfigureVisibility
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
  WidgetNode TaggerModel TaggerEvent
selectionConfigurePage =
  box . flip styleBasic [padding 80]
    . vgrid
    $ [ selectionDisplayParentsNumberField,
        spacer,
        selectionDisplayBufferSizeNumberField
      ]

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
  TaggerModel ->
  TaggerWidget
fileSelectionWidget m =
  hstack_ [] $
    [ vstack_
        []
        [ lazyBufferLoadButton,
          lazyBufferLoadAllButton,
          lazyBufferFlushButton,
          fileSelectionShuffleButton
        ],
      lazyBufferWidget (m ^. fileSelectionModel . fileSelection . buffer)
    ]
  where
    lazyBufferWidget :: [FileWithTags] -> TaggerWidget
    lazyBufferWidget =
      vscroll_ [wheelRate 50]
        . vstack_ [childSpacing_ 5]
        . map fileWithTagsWidget
      where
        fileWithTagsWidget :: FileWithTags -> TaggerWidget
        fileWithTagsWidget fwt =
          draggable fwt
            . flip styleBasic [textColor (if null . tags $ fwt then black else yuiBlue)]
            . label
            . getPathComponents
              ( m
                  ^. programConfig
                    . selectionconf
                    . selectionDisplayParents
              )
            . getPlainText
            $ fwt
    getPathComponents :: Int -> T.Text -> T.Text
    getPathComponents n p =
      let !brokenPath = T.splitOn "/" p
          !droppedDirs = length brokenPath - n
       in (!++) ((T.pack . show) droppedDirs !++ ".../")
            . T.intercalate "/"
            . drop droppedDirs
            $ brokenPath

fileSingleWidget ::
  TaggerModel -> TaggerWidget
fileSingleWidget m =
  flip styleBasic [maxHeight 10000]
    . box_ [alignTop, alignMiddle]
    . hsplit_ [splitIgnoreChildResize True]
    $ ( imagePreview . fmap getPlainText $ (m ^. singleFileModel . singleFile),
        imageDetailWidget m
      )
  where
    imagePreview ::
      (WidgetModel s, HasDoSoloTag s Bool) =>
      Maybe T.Text ->
      WidgetNode s TaggerEvent
    imagePreview mt =
      box_ [onClick ToggleDoSoloTag]
        . maybeDraggable [transparency 0.3] (m ^. singleFileModel . singleFile)
        . flip dropTarget_ [] (DoSingleFileEvent . SingleFilePut)
        . zstack
        $ [ maybe
              (label "No Preview")
              ( flip styleBasic [paddingB 3, paddingT 3]
                  . flip image_ [fitHeight, alignCenter]
              )
              $ mt,
            label . T.pack . show $ mt
          ]
      where
        maybeDraggable ss = maybe id (`draggable_` ss)

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
