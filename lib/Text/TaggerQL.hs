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
import Data.Tagger
import qualified Data.Text as T
import Database.Tagger
import Text.TaggerQL.AST

{- |
 Given a TaggerQL query, produce a set of the 'File`s it corresponds to.
-}
runTaggerQL :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
runTaggerQL = undefined

{- |
 Given a TaggerQL query, produce the abstract syntax tree.
-}
taggerQLAST :: T.Text -> TaggerQLQuery T.Text
taggerQLAST = undefined

{- |
 Return a set of files that the given 'TaggerQLSimpleTerm` corresponds to.
-}
queryWithTaggerQLSimpleTerm ::
  TaggedConnection -> TaggerQLSimpleTerm T.Text -> IO (HashSet.HashSet File)
queryWithTaggerQLSimpleTerm tc (TaggerQLSimpleTerm qc p) =
  HashSet.fromList
    <$> case qc of
      DescriptorCriteria -> do
        ds <- queryForDescriptorByPattern p tc
        fmap concat . mapM (flip flatQueryForFileByTagDescriptor tc . descriptorId) $ ds
      MetaDescriptorCriteria -> do
        ds <- queryForDescriptorByPattern p tc
        fmap concat . mapM (flip flatQueryForFileOnMetaRelation tc . descriptorId) $ ds
      FilePatternCriteria -> queryForFileByPattern p tc
      UntaggedCriteria -> queryForUntaggedFiles tc