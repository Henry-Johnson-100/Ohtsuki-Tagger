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
  FileSelectionEvent (RenameFile),
  FocusedFileEvent (
    CommitTagText,
    DeleteTag,
    MoveTag,
    TagFile
  ),
  TaggerEvent (
    AppendText,
    DoFileSelectionEvent,
    DoFocusedFileEvent,
    Mempty,
    NextHistory,
    PrevHistory,
    ToggleVisibilityLabel,
    Unit
  ),
 )
import Data.HierarchyMap (HierarchyMap)
import qualified Data.HierarchyMap as HM
import qualified Data.List as L
import Data.Model.Core (TaggerModel, focusedFileDefaultRecordKey)
import Data.Model.Lens (
  HasFileInfoRenameText (fileInfoRenameText),
  HasFileSelectionInfoMap (fileSelectionInfoMap),
  HasFileSelectionModel (fileSelectionModel),
  HasFocusedFile (focusedFile),
  HasFocusedFileModel (focusedFileModel),
  HasFocusedFileVis (focusedFileVis),
  TaggerLens (TaggerLens),
  fileInfoAt,
  tagInput,
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
  ConcreteTag (ConcreteTag, concreteTagDescriptor, concreteTagId),
  ConcreteTaggedFile (ConcreteTaggedFile, concreteTaggedFile),
  Descriptor (Descriptor, descriptor),
  File (fileId, filePath),
  RecordKey,
  Tag,
 )
import Interface.Theme (yuiBlue, yuiLightPeach, yuiPeach, yuiRed)
import Interface.Widget.Internal.Core (
  defaultElementOpacity,
  defaultOpacityModulator,
  modulateOpacity,
  styledButton_,
  withNodeKey,
  withNodeVisible,
  withStyleBasic,
 )
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  CmbAcceptTab (acceptTab),
  CmbAlignLeft (alignLeft),
  CmbAlignTop (alignTop),
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbMaxHeight (maxHeight),
  CmbOnChange (onChange),
  CmbPaddingR (paddingR),
  CmbResizeFactor (resizeFactor),
  CmbStyleHover (styleHoverSet),
  CmbTextColor (textColor),
  CmbWheelRate (wheelRate),
  CmbWidth (width),
  box_,
  buttonD_,
  draggable,
  dropTargetStyle,
  dropTarget_,
  hstack,
  hstack_,
  keystroke_,
  label,
  label_,
  separatorLine,
  spacer,
  spacer_,
  textArea_,
  textField_,
  vscroll_,
  vstack,
  vstack_,
  zstack_,
 )
import Util (compareConcreteTags)

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
            [ imageTagsWidget hm
            , tagTextField
            , deleteTagZone
            ]
        ]

imageTagsWidget ::
  HierarchyMap ConcreteTag ->
  TaggerWidget
imageTagsWidget hm =
  vscroll_ [wheelRate 50]
    . vstack
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
    [ withNodeVisible
        ( focusedFileDefaultRecordKey
            /= (fileId . concreteTaggedFile $ m ^. focusedFileModel . focusedFile)
        )
        $ styledButton_
          [resizeFactor (-1)]
          "Rename"
          ( ToggleVisibilityLabel
              (TaggerLens $ focusedFileModel . focusedFileVis)
              fileRenameModeVis
          )
    , zstack_
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

deleteTagZone :: TaggerWidget
deleteTagZone =
  dropTarget_
    (DoFocusedFileEvent . DeleteTag . concreteTagId)
    [dropTargetStyle [border 1 yuiRed]]
    . flip styleHoverSet []
    . withStyleBasic
      [ bgColor
          . modulateOpacity
            (defaultElementOpacity - defaultOpacityModulator)
          $ yuiLightPeach
      , border 1
          . modulateOpacity
            (defaultElementOpacity - defaultOpacityModulator)
          $ yuiPeach
      ]
    $ buttonD_ "Delete" [resizeFactor (-1)]

tagTextNodeKey :: Text
tagTextNodeKey = "tag-text-field"

tagTextField :: TaggerWidget
tagTextField =
  keystroke_
    [ ("Shift-Enter", DoFocusedFileEvent CommitTagText)
    , ("Shift-Up", NextHistory $ TaggerLens (focusedFileModel . tagInput))
    , ("Shift-Down", PrevHistory $ TaggerLens (focusedFileModel . tagInput))
    ]
    []
    . dropTarget_
      ( AppendText (TaggerLens $ focusedFileModel . tagInput . text)
          . descriptor
          . concreteTagDescriptor
      )
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      (AppendText (TaggerLens $ focusedFileModel . tagInput . text) . descriptor)
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey tagTextNodeKey
    . withStyleBasic
      [ bgColor
          . modulateOpacity
            (defaultElementOpacity - defaultOpacityModulator)
          $ yuiLightPeach
      , maxHeight 250
      ]
    $ textArea_
      (focusedFileModel . tagInput . text)
      [ onChange
          ( \t ->
              if T.null . T.strip $ t
                then
                  Mempty $
                    TaggerLens
                      ( focusedFileModel
                          . tagInput
                          . history
                          . historyIndex
                      )
                else Unit ()
          )
      , acceptTab
      ]
