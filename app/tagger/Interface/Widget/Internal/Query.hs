{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Interface.Widget.Internal.Query (
  queryTextFieldKey,
  widget,
) where

import Control.Lens
import Data.Event
import Data.Model
import Data.Model.Shared (HasHistoryIndex (historyIndex))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Interface.Theme
import Interface.Widget.Internal.Core
import Interface.Widget.Internal.Type (TaggerWidget)
import Monomer
import Monomer.Graphics.Lens

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
      (fileSelectionModel . queryText)
      [ onChange
          ( \t ->
              if T.null . T.strip $ t
                then Mempty $ TaggerLens (fileSelectionModel . queryHistory . historyIndex)
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