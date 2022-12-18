{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Interface.Widget.Internal.Query (
  queryTextFieldKey,
  widget,
) where

import Control.Lens ((&), (.~))
import Data.Event (
  FileSelectionEvent (
    AppendQueryText,
    NextQueryHist,
    PrevQueryHist,
    Query
  ),
  TaggerEvent (DoFileSelectionEvent, IOEvent, Mempty),
 )
import Data.Model.Core (TaggerModel)
import Data.Model.Lens (
  HasFileSelectionModel (fileSelectionModel),
  HasQueryInput (queryInput),
  TaggerLens (TaggerLens),
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
  withNodeKey,
  withStyleBasic,
 )
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer (
  CmbAlignBottom (alignBottom),
  CmbAlignLeft (alignLeft),
  CmbBgColor (bgColor),
  CmbBorder (border),
  CmbIgnoreEmptyArea (ignoreEmptyArea),
  CmbOnChange (onChange),
  CmbPaddingL (paddingL),
  CmbPaddingT (paddingT),
  box_,
  dropTargetStyle,
  dropTarget_,
  keystroke_,
  textField_,
 )
import Monomer.Graphics.Lens (HasA (a))

widget :: TaggerModel -> TaggerWidget
widget _ =
  container
    queryTextField

queryTextField :: TaggerWidget
queryTextField =
  keystroke_
    [ ("Enter", DoFileSelectionEvent Query)
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
      (fileSelectionModel . queryInput . text)
      [ onChange
          ( \t ->
              if T.null . T.strip $ t
                then
                  Mempty $
                    TaggerLens
                      ( fileSelectionModel
                          . queryInput
                          . history
                          . historyIndex
                      )
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