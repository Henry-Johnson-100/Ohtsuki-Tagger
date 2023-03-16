{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.Query (
  queryTextFieldKey,
  widget,
) where

import Data.Event (
  QueryEvent (RunQuery),
  TaggerEvent (
    AppendText,
    DoQueryEvent,
    Mempty,
    NextHistory,
    PrevHistory,
    Unit
  ),
 )
import Data.Model (
  HasFileSelectionModel (fileSelectionModel),
  HasInput (input),
  HasQueryModel (queryModel),
  TaggerLens (TaggerLens),
  TaggerModel,
 )
import Data.Model.Shared.Lens (
  HasHistoryIndex (historyIndex),
  history,
  text,
 )
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger (
  ConcreteTag (concreteTagDescriptor),
  Descriptor (descriptor),
  File (filePath),
 )
import Interface.Theme (
  yuiBlue,
  yuiLightPeach,
  yuiOrange,
  yuiRed,
 )
import Interface.Widget.Internal.Core (
  defaultElementOpacity,
  defaultOpacityModulator,
  modulateOpacity,
  withNodeKey,
  withStyleBasic,
 )
import Monomer (
  WidgetNode,
  acceptTab,
  alignBottom,
  alignLeft,
  bgColor,
  border,
  box_,
  dropTargetStyle,
  dropTarget_,
  height,
  ignoreChildrenEvts,
  ignoreEmptyArea,
  keystroke_,
  onChange,
  paddingL,
  paddingT,
  textArea_,
  tooltipDelay,
  tooltip_,
 )

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

widget :: TaggerModel -> TaggerWidget
widget _ =
  container
    queryTextField

queryTextField :: TaggerWidget
queryTextField =
  withStyleBasic
    [ bgColor
        . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
        $ yuiLightPeach
    ]
    . tooltip_ "Shift-Enter to run query." [tooltipDelay 1500]
    . keystroke_
      [ ("Shift-Enter", DoQueryEvent RunQuery)
      , ("Shift-Up", NextHistory $ TaggerLens (fileSelectionModel . queryModel . input))
      , ("Shift-Down", PrevHistory $ TaggerLens (fileSelectionModel . queryModel . input))
      ]
      [ignoreChildrenEvts]
    . dropTarget_
      ( AppendText (TaggerLens $ fileSelectionModel . queryModel . input . text)
          . descriptor
          . concreteTagDescriptor
      )
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      ( AppendText
          ( TaggerLens $
              fileSelectionModel . queryModel . input . text
          )
          . filePath
      )
      [dropTargetStyle [border 1 yuiOrange]]
    . dropTarget_
      ( AppendText
          ( TaggerLens $
              fileSelectionModel . queryModel . input . text
          )
          . descriptor
      )
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey queryTextFieldKey
    . withStyleBasic
      [ bgColor
          . modulateOpacity (defaultElementOpacity - defaultOpacityModulator)
          $ yuiLightPeach
      , height 250
      ]
    $ textArea_
      (fileSelectionModel . queryModel . input . text)
      [ onChange
          ( \t ->
              if T.null . T.strip $ t
                then
                  Mempty $
                    TaggerLens
                      ( fileSelectionModel
                          . queryModel
                          . input
                          . history
                          . historyIndex
                      )
                else Unit ()
          )
      , acceptTab
      ]

container :: TaggerWidget -> TaggerWidget
container w =
  withStyleBasic [paddingT 5, paddingL 10]
    . box_ [alignLeft, alignBottom, ignoreEmptyArea]
    $ w

queryTextFieldKey :: Text
queryTextFieldKey = "queryTextField"
{-# INLINE queryTextFieldKey #-}
