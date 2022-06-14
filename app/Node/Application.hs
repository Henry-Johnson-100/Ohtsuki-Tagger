{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant flip" #-}
{-# HLINT ignore "Redundant $" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}

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
    globalKeystrokes,
  )
where

import Control.Lens ((^.))
import qualified Data.Text as T
import Database.Tagger.Type (DescriptorTree)
import IO (taggerVersion)
import Monomer
  ( AppConfig,
    CmbAlignCenter (alignCenter),
    CmbAlignLeft (alignLeft),
    CmbAlignMiddle (alignMiddle),
    CmbAlignTop (alignTop),
    CmbBgColor (bgColor),
    CmbBorder (border),
    CmbFitHeight (fitHeight),
    CmbIgnoreChildrenEvts (ignoreChildrenEvts),
    CmbMaxHeight (maxHeight),
    CmbOnClick (onClick),
    CmbPadding (padding),
    CmbPaddingB (paddingB),
    CmbPaddingT (paddingT),
    CmbStyleBasic (styleBasic),
    CmbTextSize (textSize),
    CmbTransparency (transparency),
    MainWindowState (MainWindowMaximized, MainWindowNormal),
    Theme,
    WidgetModel,
    WidgetNode,
    appDisposeEvent,
    appFontDef,
    appScaleFactor,
    appTheme,
    appWindowIcon,
    appWindowState,
    appWindowTitle,
    black,
    box,
    box_,
    button,
    draggable_,
    dropTarget_,
    hgrid,
    hgrid_,
    hsplit_,
    hstack,
    hstack_,
    image_,
    keystroke_,
    label,
    lightThemeColors,
    nodeVisible,
    separatorLine,
    spacer,
    splitIgnoreChildResize,
    vgrid,
    vgrid_,
    vstack,
    vstack_,
    white,
    zstack,
  )
import Monomer.Core.Themes.BaseTheme
  ( BaseThemeColors
      ( btnBgActive,
        btnBgBasic,
        btnBgFocus,
        btnBgHover,
        btnFocusBorder,
        clearColor,
        dialogBg,
        inputBgBasic,
        inputBgFocus,
        inputFocusBorder,
        inputSelBasic,
        inputSelFocus,
        sectionColor,
        slMainBg,
        slNormalBgHover,
        slNormalFocusBorder,
        slSelectedBgBasic,
        slSelectedBgHover,
        slSelectedFocusBorder
      ),
    baseTheme,
  )
import Node.Color
  ( yuiLightPeach,
    yuiOrange,
    yuiPeach,
    yuiRed,
    yuiYellow,
  )
import Node.Micro
  ( GetPlainText (getPlainText),
    TaggerWidget,
    buttonStylingBasic,
    clearSelectionButton,
    configurationExportButton,
    databaseBackupButton,
    databaseConnectButton,
    dbAutoConnectCheckBox,
    dbBackupTextField,
    dbPathTextField,
    descriptorDeleteWidget,
    descriptorNewTextField,
    descriptorTreeConfigureMainRequestTextField,
    doShellCmdButton,
    fileSelectionShuffleButton,
    fileSingleNextFromFileSelectionButton,
    fileSinglePrevFromFileSelectionButton,
    generalDescriptorTreeWidget,
    getPathComponents,
    imageDetailWidget,
    initializeDatabaseButton,
    labeledWidget,
    lazyBufferFlushButton,
    lazyBufferLoadAllButton,
    lazyBufferLoadButton,
    mainDescriptorTreeWidget,
    newFileTextCommitButton,
    newFileTextField,
    parentDescriptorTreeButton,
    queryTextField,
    renameDescriptorWidget,
    representativeFilePreview,
    resetDescriptorTreeToButton,
    selectionDisplayBufferSizeNumberField,
    selectionDisplayParentsNumberField,
    setArithmeticDropdown,
    setQueryCriteriaDropdown,
    shellCmdTextField,
    stdDelayTooltip,
    taggingModeDropdown,
    tagsStringTextField,
    toggleConfigConfigureVisibility,
    toggleDatabaseConfigureVisibility,
    toggleDescriptorConfigureVisibility,
    toggleSelectionConfigureVisibility,
    treeLeafButtonRequestDescriptorTree,
    unrelatedDescriptorTreeWidget,
  )
import Type.BufferList (buffer, totalBufferList)
import Type.Config
  ( DescriptorTreeConfig,
    FontConfig (bold, regular, thin),
    StyleConfig (font, window),
    WindowConfig
      ( maximize,
        windowIcon,
        windowScalingFactor,
        windowSizeX,
        windowSizeY
      ),
  )
import Type.Model
  ( FileSelectionEvent
      ( FileSelectionClear,
        FileSelectionCommitQueryText,
        FileSelectionNextQueryCriteria,
        FileSelectionNextSetArithmetic,
        FileSelectionPrevQueryCriteria,
        FileSelectionPrevSetArithmetic
      ),
    HasDbConn (dbConn),
    HasDescriptorModel (descriptorModel),
    HasDoSoloTag,
    HasFileSelection (fileSelection),
    HasFileSelectionModel (fileSelectionModel),
    HasLastAccessed (lastAccessed),
    HasLastBackup (lastBackup),
    HasMainDescriptorTree (mainDescriptorTree),
    HasNewDescriptorText,
    HasProgramConfig (programConfig),
    HasProgramVisibility (..),
    HasRepresentativeFile (representativeFile),
    HasSingleFile (singleFile),
    HasSingleFileModel (singleFileModel),
    ProgramVisibility,
    SingleFileEvent
      ( SingleFileNextFromFileSelection,
        SingleFilePrevFromFileSelection,
        SingleFilePut
      ),
    TaggerEvent
      ( DatabaseClose,
        DescriptorCommitNewDescriptorText,
        DoFileSelectionEvent,
        DoSingleFileEvent,
        RefreshApplication,
        ShellCmd,
        TagCommitTagsString,
        TaggingModeNext,
        TaggingModePrev,
        ToggleDoSoloTag
      ),
    TaggerModel,
    descriptorTreeConf,
    rootTree,
    selectionDisplayParents,
    selectionconf,
  )
import Util.Core ((!++))

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

themeConfig :: StyleConfig -> [AppConfig TaggerEvent]
themeConfig cfg =
  [ appWindowTitle "Tagger",
    appWindowState $
      if maximize . window $ cfg
        then MainWindowMaximized
        else
          MainWindowNormal
            ( fromIntegral . windowSizeX . window $ cfg,
              fromIntegral . windowSizeY . window $ cfg
            ),
    appScaleFactor . windowScalingFactor . window $ cfg,
    appTheme yuiTheme,
    appFontDef "Regular" (regular . font $ cfg),
    appFontDef "Thin" (thin . font $ cfg),
    appFontDef "Bold" (bold . font $ cfg),
    appDisposeEvent DatabaseClose
  ]
    ++ maybe [] ((: []) . appWindowIcon) (windowIcon . window $ cfg)

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
    [ labeledWidget "Add files to database:"
        . hstack
        $ [newFileTextCommitButton, newFileTextField],
      spacer,
      labeledWidget
        "Database to load:"
        dbPathTextField,
      spacer,
      labeledWidget
        "Database to backup to:"
        dbBackupTextField,
      spacer,
      dbAutoConnectCheckBox,
      databaseConnectButton,
      databaseBackupButton,
      spacer,
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

configConfigurationPage :: TaggerModel -> TaggerWidget
configConfigurationPage m =
  box . flip styleBasic [padding 80]
    . vgrid
    $ [ configurationExportButton,
        label ("Tagger Version: " !++ T.pack taggerVersion),
        label ("Last Accessed: " !++ (m ^. dbConn . lastAccessed)),
        label ("Last Backup: " !++ (m ^. dbConn . lastBackup))
      ]

descriptorConfigurePage :: TaggerModel -> WidgetNode TaggerModel TaggerEvent
descriptorConfigurePage model =
  box . flip styleBasic [padding 80]
    . vstack
    $ [ labeledWidget "Main Tree Root" descriptorTreeConfigureMainRequestTextField,
        spacer,
        renameDescriptorWidget,
        spacer,
        representativeFilePreview
          (model ^. descriptorModel . representativeFile),
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
  flip styleBasic [border 1 black]
    . vstack_ []
    $ [ label
          ( "("
              !++ ( T.pack . show . length $
                      m ^. fileSelectionModel . fileSelection . buffer
                  )
              !++ " / "
              !++ ( T.pack . show . length . totalBufferList $
                      m ^. fileSelectionModel . fileSelection
                  )
              !++ ")"
          )
          `styleBasic` [paddingT 2],
        separatorLine,
        hstack_
          []
          [ lazyBufferWidget (m ^. fileSelectionModel . fileSelection . buffer),
            vstack_
              []
              [ lazyBufferLoadButton,
                lazyBufferLoadAllButton,
                lazyBufferFlushButton,
                fileSelectionShuffleButton
              ]
          ]
      ]
  where
    lazyBufferWidget = const (label "Lazy Buffer Widget WIP") -- #FIXME
    -- lazyBufferWidget :: [FileWithTags] -> TaggerWidget
    -- lazyBufferWidget =
    --   flip styleBasic [maxWidth 10000]
    --     . vscroll_ [wheelRate 50]
    --     . vstack_ [childSpacing_ 5]
    --     . map
    --       ( fileWithTagsWidget
    --           (m ^. programConfig . selectionconf . selectionDisplayParents)
    --       )
    --   where
    --     fileWithTagsWidget :: Int -> FileWithTags -> TaggerWidget
    --     fileWithTagsWidget n fwt =
    --       draggable fwt
    --         . flip styleBasic [textColor (if null . tags $ fwt then black else yuiBlue)]
    --         . flip label_ [ellipsis]
    --         . getPathComponents n
    --         . getPlainText
    --         $ fwt

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
              $ mt
          ]
          ++ maybe
            []
            ( (: [])
                . box_ [alignTop, alignLeft]
                . flip styleBasic [bgColor white]
                . label
                . getPathComponents
                  ( m
                      ^. programConfig
                        . selectionconf
                        . selectionDisplayParents
                  )
            )
            mt
      where
        maybeDraggable ss = maybe id (`draggable_` ss)

globalKeystrokes :: TaggerWidget -> TaggerWidget
globalKeystrokes =
  flip
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
      ("Ctrl-Shift-o", TaggingModePrev),
      ("Ctrl-r", RefreshApplication),
      ("Ctrl-t", ToggleDoSoloTag)
    ]

operationWidget ::
  WidgetNode TaggerModel TaggerEvent
operationWidget =
  flip styleBasic [border 1 black]
    . box_ [alignTop]
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
    labeledShellCmdTextField :: TaggerWidget
    labeledShellCmdTextField =
      flip
        keystroke_
        [ignoreChildrenEvts]
        [("Enter", ShellCmd)]
        . labeledWidget "Shell"
        . hstack_ []
        $ [button "→" ShellCmd, shellCmdTextField]
    selectionOperatorWidget ::
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
