module Data.Event (
  TaggerEvent (..),
) where

data TaggerEvent
  = TogglePreviewFocus
  | ToggleTagMode
  | CloseConnection
  deriving (Show, Eq)