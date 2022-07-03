{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Data.Model.Shared.Core (
  Visibility (..),
  toggleAltVis,
) where

import Data.Text (Text)

{- |
 Generic data type for changing visibility of a widget.

 Provides labels for visibility for a main page and alternate page and
 two additional constructors for either numbered pages or labeled pages.
-}
data Visibility
  = VisibilityMain
  | VisibilityAlt
  | VisibilityNum Int
  | VisibilityLabel Text
  deriving (Show, Eq)

{- |
 Switches between Main and Alt visibility.

 Does nothing if the visibility is not either of these two constructors.
-}
toggleAltVis :: Visibility -> Visibility
toggleAltVis VisibilityAlt = VisibilityMain
toggleAltVis VisibilityMain = VisibilityAlt
toggleAltVis x = x