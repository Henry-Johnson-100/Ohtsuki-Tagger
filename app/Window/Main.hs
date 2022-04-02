{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Window.Main () where

import Control.Lens
import Window.Core

data AppModel = AppModel
  { _selectionFiles :: [Text]
  }
  deriving (Show, Eq)

data AppEvent = AppInit deriving (Show, Eq)

makeLensesWith abbreviatedFields ''AppModel