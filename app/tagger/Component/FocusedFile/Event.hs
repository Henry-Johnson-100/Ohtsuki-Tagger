module Component.FocusedFile.Event (
  FocusedFileEvent (..),
) where

import Database.Tagger.Type

data FocusedFileEvent
  = PutConcreteFile ConcreteTaggedFile
  | RequestConcreteFile File
  | RequestTagOccurrences
  deriving (Show, Eq)
