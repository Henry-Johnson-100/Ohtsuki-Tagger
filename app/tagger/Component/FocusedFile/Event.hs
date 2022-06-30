module Component.FocusedFile.Event (
  FocusedFileEvent (..),
) where

import Data.OccurrenceMap (OccurrenceMap)
import Database.Tagger.Type

data FocusedFileEvent
  = PutConcreteFile ConcreteTaggedFile
  | PutTagOccurrences OccurrenceMap
  | RequestConcreteFile File
  deriving (Show, Eq)
