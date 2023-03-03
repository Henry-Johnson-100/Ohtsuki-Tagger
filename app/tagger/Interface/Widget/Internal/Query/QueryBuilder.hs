{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.Widget.Internal.Query.QueryBuilder (
  expressionWidget,
  queryEditorTextFieldKey,
) where

import Data.Event
import Data.Model
import Data.Text

import Monomer
import Text.TaggerQL.Expression.AST

type TaggerWidget = WidgetNode TaggerModel TaggerEvent

-- testquery:
-- o%yui {(gym_clothes | swimsuit | kimono){blue} hair{r%bow}! (suggestive | sweetie_cute_new_year {+})}

queryEditorTextFieldKey :: Text
queryEditorTextFieldKey = "queryEditorTextField"

expressionWidget :: QueryExpression -> TaggerWidget
expressionWidget = const (label "Starting over lmao")
