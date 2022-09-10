{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.Query (
  queryTextFieldKey,
  widget,
) where

import Control.Lens
import Data.Event
import Data.Model
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Interface.Handler.WidgetQueryRequest
import Interface.Theme
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer
import Monomer.Graphics.Lens

widget :: TaggerModel -> TaggerWidget
widget m =
  container . vstack_ [] $
    [queryBuilderWidget m, queryTextField]

queryTextField :: TaggerWidget
queryTextField =
  keystroke_
    [ ("Enter", DoFileSelectionEvent CreateNewWidgetQueryNode)
    , ("Shift-Enter", DoFileSelectionEvent RunQuerySequence)
    , ("Up", DoFileSelectionEvent NextQueryHist)
    , ("Down", DoFileSelectionEvent PrevQueryHist)
    ]
    []
    . dropTarget_
      (DoFileSelectionEvent . AppendQueryText . descriptor . concreteTagDescriptor)
      [dropTargetStyle [border 1 yuiRed]]
    . dropTarget_
      (DoFileSelectionEvent . AppendQueryText . filePath)
      [dropTargetStyle [border 1 yuiOrange]]
    . dropTarget_
      (DoFileSelectionEvent . AppendQueryText . descriptor)
      [dropTargetStyle [border 1 yuiBlue]]
    . withNodeKey queryTextFieldKey
    . withStyleBasic [bgColor (yuiLightPeach & a .~ defaultElementOpacity)]
    $ textField_
      (fileSelectionModel . queryText)
      [ onChange
          ( \t ->
              if T.null . T.strip $ t
                then DoFileSelectionEvent ResetQueryHistIndex
                else IOEvent ()
          )
      ]

container :: TaggerWidget -> TaggerWidget
container w =
  withStyleBasic [paddingT 5, paddingL 10]
    . box_ [alignLeft, alignBottom, ignoreEmptyArea]
    -- . withStyleBasic [borderT 1 black, borderB 1 black, borderR 1 black]
    $ w

queryTextFieldKey :: Text
queryTextFieldKey = "queryTextField"
{-# INLINE queryTextFieldKey #-}

queryBuilderWidget :: TaggerModel -> TaggerWidget
queryBuilderWidget m =
  vstack_
    []
    ( queryBuilderNode
        <$> widgetQueryRequest
          (m ^. fileSelectionModel . fileSelectionQueryWidgetRequest)
    )

queryBuilderNode :: WidgetQueryNode -> TaggerWidget
queryBuilderNode wqn@(WidgetQueryNodeComp _ (formatSentenceTree -> tst) isNull _) =
  withStyleBasic
    [ bgColor
        . modulateOpacity
          (defaultElementOpacity - defaultOpacityModulator)
        $ if isNull then yuiRed else yuiLightPeach
    ]
    $ hstack_
      []
      [ styledButton_
          [resizeFactor (-1)]
          "-"
          (DoFileSelectionEvent . DeleteQueryNode $ wqn)
      , dropTarget_
          (DoFileSelectionEvent . flip MoveQueryNodeBefore wqn)
          [ dropTargetStyle
              [ borderT 1
                  . modulateOpacity
                    (defaultElementOpacity - defaultOpacityModulator)
                  $ yuiOrange
              ]
          ]
          . draggable wqn
          $ label_
            tst
            [resizeFactor (-1), ellipsis]
      ]
queryBuilderNode _ = label_ "Weird _ pattern in queryBuilderNode" [resizeFactor (-1)]
