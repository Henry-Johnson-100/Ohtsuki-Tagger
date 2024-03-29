{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.FileDetail (
  widget,
  tagTextNodeKey,
) where

import Control.Lens ((^.))
import Data.Event (
  FileSelectionEvent (ClearTaggingSelection, RenameFile),
  FocusedFileEvent (DeleteTag, MoveTag, TagFile),
  TagInputEvent (RunTagExpression, ToggleTagInputOptionPane),
  TaggerEvent (
    AppendText,
    DoFileSelectionEvent,
    DoFocusedFileEvent,
    DoTagInputEvent,
    Mempty,
    NextHistory,
    PrevHistory,
    Unit
  ),
 )
import Data.HierarchyMap (HierarchyMap)
import qualified Data.HierarchyMap as HM
import qualified Data.List as L
import Data.Model.Core (TaggerModel, tagInputOptionPaneLabel)
import Data.Model.Lens (
  HasFileInfoRenameText (fileInfoRenameText),
  HasFileSelectionInfoMap (fileSelectionInfoMap),
  HasFileSelectionModel (fileSelectionModel),
  HasFocusedFile (focusedFile),
  HasFocusedFileModel (focusedFileModel),
  HasFocusedFileVis (focusedFileVis),
  HasInput (input),
  HasTagInputModel (tagInputModel),
  TaggerLens (TaggerLens),
  fileInfoAt,
  isTagDelete,
  isTagSelection,
  visibility,
 )
import Data.Model.Shared.Core (
  Visibility (VisibilityLabel),
  hasVis,
 )
import Data.Model.Shared.Lens (
  HasHistoryIndex (historyIndex),
  history,
  text,
 )
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  ConcreteTag (ConcreteTag, concreteTagDescriptor),
  ConcreteTaggedFile (ConcreteTaggedFile, concreteTaggedFile),
  Descriptor (Descriptor, descriptor),
  File (fileId, filePath),
  RecordKey,
  Tag,
  concreteTagId,
 )
import Interface.Theme (
  yuiBlue,
  yuiLightPeach,
  yuiRed,
 )
import Interface.Widget.Internal.Core (
  defaultElementOpacity,
  defaultOpacityModulator,
  modulateOpacity,
  styledButton_,
  styledToggleButton_,
  withNodeKey,
  withNodeVisible,
  withStyleBasic,
 )
import Monomer (
  CmbAcceptTab (acceptTab),
  CmbAlignLeft (alignLeft),
  CmbAlignTop (alignTop),
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbIgnoreChildrenEvts (ignoreChildrenEvts),
  CmbOnChange (onChange),
  CmbPaddingR (paddingR),
  CmbResizeFactor (resizeFactor),
  CmbTextColor (textColor),
  CmbWheelRate (wheelRate),
  CmbWidth (width),
  WidgetNode,
  alignBottom,
  alignMiddle,
  box_,
  draggable,
  dropTargetStyle,
  dropTarget_,
  height,
  hstack,
  hstack_,
  keystroke_,
  label,
  label_,
  resizeFactorW,
  separatorLine,
  separatorLine_,
  spacer,
  spacer_,
  textArea_,
  textField_,
  tooltipDelay,
  tooltip_,
  vscroll_,
  vstack,
  vstack_,
  zstack_,
 )
import Util (compareConcreteTags)

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

widget :: TaggerModel -> TaggerWidget
widget m = detailPane m

detailPane :: TaggerModel -> TaggerWidget
detailPane m =
  hstack_
    []
    [ separatorLine
    , detailPaneTagsWidget m
    ]

detailPaneTagsWidget :: TaggerModel -> TaggerWidget
detailPaneTagsWidget
  m@( (^. focusedFileModel . focusedFile) ->
        (ConcreteTaggedFile _ hm)
      ) =
    withStyleBasic
      [paddingR 20]
      $ vstack_
        []
        [ filePathWidget m
        , separatorLine
        , vstack
            [ taggingWidget m
            , separatorLine_ [resizeFactor (-1)]
            , imageTagsWidget hm
            ]
        ]

imageTagsWidget ::
  HierarchyMap ConcreteTag ->
  TaggerWidget
imageTagsWidget hm =
  box_ []
    . vscroll_ [wheelRate 50]
    . (\xs -> case xs of [] -> spacer; _notNull -> vstack xs)
    . HM.traverseHierarchyMap
      ()
      id
      ( \_ ct@(ConcreteTag tk (Descriptor _ dp) _) children ->
          vstack
            [ hstack
                [ subTagDropTarget tk . box_ [alignLeft, alignTop]
                    . draggable ct
                    . withStyleBasic [textColor yuiBlue]
                    $ label dp
                , spacer
                , label "{"
                ]
            , hstack
                [ metaTagLeafSpacer
                , box_ [alignLeft, alignTop] . vstack $ children
                ]
            , label "}"
            ]
      )
      ( \_ ct@(ConcreteTag tk (Descriptor _ dp) _) ->
          subTagDropTarget tk
            . box_ [alignLeft, alignTop]
            . draggable ct
            $ label dp
      )
      (L.sortBy (compareConcreteTags hm))
    $ hm

filePathWidget :: TaggerModel -> TaggerWidget
filePathWidget m =
  hstack_
    []
    [ zstack_
        []
        [ withNodeVisible (not $ isFileRenameMode m) $
            label_
              (filePath . concreteTaggedFile $ m ^. focusedFileModel . focusedFile)
              [resizeFactor (-1)]
        , withNodeVisible (isFileRenameMode m)
            . keystroke_
              [
                ( "Enter"
                , DoFileSelectionEvent
                    . RenameFile
                    $ ( fileId . concreteTaggedFile $
                          m ^. focusedFileModel . focusedFile
                      )
                )
              ]
              []
            . withStyleBasic
              [ bgColor
                  . modulateOpacity
                    (defaultElementOpacity - defaultOpacityModulator)
                  $ yuiLightPeach
              ]
            $ textField_
              ( fileSelectionModel
                  . fileSelectionInfoMap
                  . fileInfoAt
                    ( fromIntegral
                        . fileId
                        . concreteTaggedFile
                        $ m ^. focusedFileModel . focusedFile
                    )
                  . fileInfoRenameText
              )
              []
        ]
    ]

fileRenameModeVis :: Text
fileRenameModeVis = "file-rename"

isFileRenameMode :: TaggerModel -> Bool
isFileRenameMode m =
  (m ^. focusedFileModel . focusedFileVis)
    `hasVis` VisibilityLabel fileRenameModeVis
{-# INLINE isFileRenameMode #-}

metaTagLeafSpacer :: TaggerWidget
metaTagLeafSpacer = spacer_ [width 20]
{-# INLINE metaTagLeafSpacer #-}

subTagDropTarget ::
  RecordKey Tag ->
  TaggerWidget ->
  TaggerWidget
subTagDropTarget tk =
  dropTarget_
    (\(Descriptor dk _) -> DoFocusedFileEvent (TagFile dk (Just tk)))
    [dropTargetStyle [border 1 yuiBlue]]
    . dropTarget_
      ( \ct ->
          DoFocusedFileEvent
            (MoveTag ct (Just tk))
      )
      [dropTargetStyle [border 1 yuiRed]]

tagTextNodeKey :: Text
tagTextNodeKey = "tag-text-field"

taggingWidget :: TaggerModel -> TaggerWidget
taggingWidget m =
  box_ [alignBottom] $
    vstack
      [ tagTextField
      , taggingOptionsToggle
      , taggingOptionsWidget
      ]
 where
  taggingOptionsToggle =
    styledButton_
      [resizeFactorW (-1)]
      "Options"
      (DoTagInputEvent ToggleTagInputOptionPane)

  taggingOptionsWidget =
    withNodeVisible
      ( (m ^. tagInputModel . visibility)
          `hasVis` VisibilityLabel tagInputOptionPaneLabel
      )
      $ vstack_
        []
        [ box_ [alignMiddle] $
            hstack
              [ styledToggleButton_
                  [resizeFactorW (-1)]
                  "Tag Selection"
                  (tagInputModel . isTagSelection)
              , styledButton_
                  [resizeFactorW (-1)]
                  "Clear Tag Selection"
                  (DoFileSelectionEvent ClearTaggingSelection)
              ]
        , dropTarget_
            (DoFocusedFileEvent . DeleteTag . concreteTagId)
            [dropTargetStyle [border 1 yuiRed]]
            $ styledToggleButton_
              [resizeFactorW (-1)]
              "Delete Mode"
              (tagInputModel . isTagDelete)
        ]
  --  where
  --   styledToggleButton t l =
  --     withStyleBasic
  --       [ bgColor
  --           . modOpac
  --           $ yuiBlue
  --       , textColor yuiBlack
  --       ]
  --       $ toggleButton_
  --         t
  --         l
  --         [ resizeFactorW (-1)
  --         , toggleButtonOffStyle $
  --             mempty
  --               & flip styleBasic [bgColor . modOpac $ yuiLightPeach, textColor yuiBlack]
  --               & flip
  --                 styleHover
  --                 [ bgColor . modulateOpacity defaultElementOpacity $ yuiYellow
  --                 , border 1 yuiOrange
  --                 ]
  --         ]
  --    where
  --     modOpac = modulateOpacity (defaultElementOpacity - defaultOpacityModulator)

  tagTextField :: TaggerWidget
  tagTextField =
    withStyleBasic
      [ bgColor
          . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
          $ yuiLightPeach
      ]
      . tooltip_ "Shift-Enter to tag the current file." [tooltipDelay 1500]
      . keystroke_
        [ ("Shift-Enter", DoTagInputEvent RunTagExpression)
        , ("Shift-Up", NextHistory $ TaggerLens (tagInputModel . input))
        , ("Shift-Down", PrevHistory $ TaggerLens (tagInputModel . input))
        ]
        [ignoreChildrenEvts]
      . dropTarget_
        ( AppendText (TaggerLens $ tagInputModel . input . text)
            . descriptor
            . concreteTagDescriptor
        )
        [dropTargetStyle [border 1 yuiRed]]
      . dropTarget_
        (AppendText (TaggerLens $ tagInputModel . input . text) . descriptor)
        [dropTargetStyle [border 1 yuiBlue]]
      . withNodeKey tagTextNodeKey
      . withStyleBasic
        [ bgColor
            . modulateOpacity
              (defaultElementOpacity - defaultOpacityModulator)
            $ yuiLightPeach
        , height 250
        ]
      $ textArea_
        (tagInputModel . input . text)
        [ onChange
            ( \t ->
                if T.null . T.strip $ t
                  then
                    Mempty $
                      TaggerLens
                        ( tagInputModel
                            . input
                            . history
                            . historyIndex
                        )
                  else Unit ()
            )
        , acceptTab
        ]
