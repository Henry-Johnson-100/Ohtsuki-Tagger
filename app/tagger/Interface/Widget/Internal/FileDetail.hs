{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Interface.Widget.Internal.FileDetail (widget) where

import Control.Lens
import Data.Event
import qualified Data.HashSet as HS
import qualified Data.HierarchyMap as HM
import qualified Data.List as L
import Data.Model
import Data.Model.Shared
import Database.Tagger
import Interface.Theme
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type
import Monomer

widget :: TaggerModel -> TaggerWidget
widget m = detailPane m

detailPane :: TaggerModel -> TaggerWidget
detailPane m@((^. focusedFileModel . focusedFile) -> (ConcreteTaggedFile _ hm)) =
  hstack_
    []
    [ separatorLine
    , detailPaneTagsWidget
    ]
 where
  detailPaneTagsWidget =
    withStyleBasic
      [paddingR 20]
      $ vstack_
        []
        [ filePathWidget
        , separatorLine
        , vscroll_ [wheelRate 50] $
            vstack
              [ metaLeaves
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
              , deleteTagZone
              ]
        ]
   where
    filePathWidget :: TaggerWidget
    filePathWidget =
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
            [ withNodeVisible (not isFileRenameMode) $
                label_
                  (filePath . concreteTaggedFile $m ^. focusedFileModel . focusedFile)
                  [resizeFactor (-1)]
            , withNodeVisible isFileRenameMode
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
     where
      fileRenameModeVis = "file-rename"
      isFileRenameMode =
        (m ^. focusedFileModel . focusedFileVis)
          `hasVis` VisibilityLabel fileRenameModeVis
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
    metaLeaves :: [ConcreteTag] -> TaggerWidget
    metaLeaves members =
      vstack_ [] . L.intersperse spacer $
        (flip metaLeaf hm <$> members)
     where
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
       where
        metaTagLeafSpacer = spacer_ [width 20]
    deleteTagZone :: TaggerWidget
    deleteTagZone =
      dropTarget_
        (DoFocusedFileEvent . DeleteTag . concreteTagId)
        [dropTargetStyle [border 1 yuiRed]]
        . flip styleHoverSet []
        . withStyleBasic [bgColor yuiLightPeach, border 1 yuiPeach]
        $ buttonD_ "Delete" [resizeFactor (-1)]
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
