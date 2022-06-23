{- |
Module      : Text.TaggerQL
Description : The front-end and interface for running TaggerQL queries on a Tagger database.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL (
  runTaggerQL,
) where

import qualified Data.HashSet as HashSet
import Data.Tagger
import qualified Data.Text as T
import Database.Tagger
import Text.TaggerQL.AST

{- |
 Given a TaggerQL query, produce a set of the 'File`s it corresponds to.
-}
runTaggerQL :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
runTaggerQL = undefined
