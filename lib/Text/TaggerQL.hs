{- |
Module      : Text.TaggerQL
Description : The front-end and interface for running TaggerQL queries on a Tagger database.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Text.TaggerQL (
  runTaggerQL,
  taggerQLAST,
) where

import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Database.Tagger
import Text.TaggerQL.AST

runTaggerQL :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
runTaggerQL = undefined

taggerQLAST :: T.Text -> TaggerQLAST T.Text
taggerQLAST = undefined