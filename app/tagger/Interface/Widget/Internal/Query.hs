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
import Data.Model
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
    [ ("Enter", DoQueryEvent RunQuery)
    , ("Up", NextHistory $ TaggerLens (fileSelectionModel . queryModel . input))
    , ("Down", PrevHistory $ TaggerLens (fileSelectionModel . queryModel . input))
    ]
    []
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
    . withStyleBasic [bgColor (yuiLightPeach & a .~ defaultElementOpacity)]
    $ textField_
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

-- Query builder widget stuff
