{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Data.Model.Shared.Core (
  VisibilityModel (..),
  Visibility (..),
  createVisibilityModel,
) where

import Data.Text (Text)

data VisibilityModel = VisibilityModel
  { _visibilitymodelDescriptorTreeVis :: Visibility
  }
  deriving (Show, Eq)

createVisibilityModel :: VisibilityModel
createVisibilityModel =
  VisibilityModel
    { _visibilitymodelDescriptorTreeVis = VisibilityMain
    }

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
