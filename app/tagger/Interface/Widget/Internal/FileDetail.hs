{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Interface.Widget.Internal.FileDetail (
  widget,
  tagTextNodeKey,
) where

import Control.Lens hiding (both)
import Data.Event
import qualified Data.HashSet as HS
import Data.HierarchyMap (HierarchyMap)
import qualified Data.HierarchyMap as HM
import qualified Data.List as L
import Data.Model
import Data.Model.Shared
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Interface.Theme
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type
import Monomer

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
            [ vscroll_ [wheelRate 50] $
                vstack
                  [ metaLeaves
                      hm
                      ( L.sortOn (descriptor . concreteTagDescriptor)
                          . filter
                            ( \x ->
                                HM.metaMember x hm
                                  && not (HM.infraMember x hm)
                            )
                          . HM.keys
                          $ hm
                      )
                  , spacer_ [resizeFactor (-1)]
                  , nullMemberLeaves
                      ( L.sortOn (descriptor . concreteTagDescriptor)
                          . filter
                            ( \x ->
                                not (HM.metaMember x hm)
                                  && not (HM.infraMember x hm)
                            )
                          . HM.keys
                          $ hm
                      )
                  ]
            , tagTextField
            , deleteTagZone
            ]
        ]

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
          ( DoFocusedFileEvent
              (ToggleFocusedFilePaneVisibility fileRenameModeVis)
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

nullMemberLeaves ::
  Traversable t =>
  t ConcreteTag ->
  WidgetNode TaggerModel TaggerEvent
nullMemberLeaves members =
  withStyleBasic [borderB 1 black]
    . vstack_ []
    $ ( \ct@(ConcreteTag tk (Descriptor _ dp) _) ->
          subTagDropTarget tk
            . box_ [alignLeft, alignTop]
            . draggable ct
            $ label dp
      )
      <$> members

metaLeaves ::
  HierarchyMap ConcreteTag ->
  [ConcreteTag] ->
  WidgetNode TaggerModel TaggerEvent
metaLeaves hm members =
  vstack_ [] . L.intersperse spacer $
    (flip metaLeaf hm <$> members)

metaLeaf ::
  ConcreteTag ->
  HierarchyMap ConcreteTag ->
  TaggerWidget
metaLeaf l@(ConcreteTag tk (Descriptor _ dp) _) hmap =
  let subtags =
        L.sortOn (descriptor . concreteTagDescriptor)
          . HS.toList
          $ HM.find l hmap
   in if null subtags
        then
          subTagDropTarget tk . box_ [alignLeft, alignTop]
            . draggable l
            $ label dp
        else
          vstack_
            []
            [ hstack_
                []
                [ subTagDropTarget tk
                    . box_ [alignLeft, alignTop]
                    . draggable l
                    . withStyleBasic [textColor yuiBlue]
                    $ label dp
                , spacer
                , label "{"
                ]
            , hstack_
                []
                [ metaTagLeafSpacer
                , box_ [alignLeft, alignTop] $
                    vstack
                      ( flip metaLeaf hmap
                          <$> subtags
                      )
                ]
            , label "}"
            ]

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
    [ ("Enter", DoFocusedFileEvent CommitTagText)
    , ("Up", DoFocusedFileEvent NextTagHist)
    , ("Down", DoFocusedFileEvent PrevTagHist)
    ]
    []
    . dropTarget_
      (DoFocusedFileEvent . AppendTagText . descriptor . concreteTagDescriptor)
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      (DoFocusedFileEvent . AppendTagText . descriptor)
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey tagTextNodeKey
    . withStyleBasic
      [ bgColor
          . modulateOpacity
            (defaultElementOpacity - defaultOpacityModulator)
          $ yuiLightPeach
          -- bgColor (yuiLightPeach & a .~ mainPaneFloatingOpacity)
      ]
    $ textField_
      (focusedFileModel . tagText)
      [ onChange
          ( \t ->
              if T.null t
                then DoFocusedFileEvent ResetTagHistIndex
                else
                  IOEvent
                    ()
          )
      ]

-- zstackTaggingWidget :: TaggerWidget
-- zstackTaggingWidget =
--   box_ [alignLeft, ignoreEmptyArea]
--     . withStyleBasic [maxWidth 400]
--     $ hstack
--       [ vstack . (: [])
--           . withStyleBasic
--             [ bgColor $
--                 yuiLightPeach
--                   & a .~ mainPaneFloatingOpacity
--             ]
--           $ styledButton_
--             [resizeFactor (-1)]
--             "Tag"
--             ( DoFocusedFileEvent
--                 (ToggleFocusedFilePaneVisibility zstackTaggingWidgetVis)
--             )
--       , withNodeVisible
--           isVisible
--           tagTextField
--       ]
--  where
--   isVisible =
--     (m ^. focusedFileModel . focusedFileVis)
--       `hasVis` VisibilityLabel zstackTaggingWidgetVis