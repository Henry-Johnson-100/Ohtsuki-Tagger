{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Module      : Database.Tagger.Query.Basic
Description : Contains basic queries.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

Contains basic queries such as searching for files by file pattern or searching for
Descriptor by Relation.
-}
module Database.Tagger.Query.Basic (
  queryForFileByPattern,
  flatQueryForFileByTagDescriptor,
) where

import qualified Data.HashSet as HashSet
import qualified Data.Text as T
import Database.Tagger.Connection
import Database.Tagger.Type
import Text.RawString.QQ

{- |
 Performs a case-insensitive search of all registered file paths, tagged or not.

 Can use SQL wildcards like % or _
-}
queryForFileByPattern :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
queryForFileByPattern p tc =
  HashSet.fromList <$> query tc q [p]
 where
  q =
    [r|
      SELECT
        id
        ,filePath
      FROM
        File
      WHERE
        filePath LIKE ?
      |]

{- |
 Performs a case-insensitive search for files that are tagged with a descriptor
 matching the text pattern provided.

 A flat search, meaning that any descriptor that tags an image will be searched,
 regardless of whether or not it is a subtag or not.
-}
flatQueryForFileByTagDescriptor :: T.Text -> TaggedConnection -> IO (HashSet.HashSet File)
flatQueryForFileByTagDescriptor p tc =
  HashSet.fromList <$> query tc q [p]
 where
  q =
    [r|
      SELECT
        f.id
        ,f.filePath
      FROM Tag t
        JOIN Descriptor d
          ON t.descriptorId = d.id
        JOIN File f
          ON t.fileId = f.id
      WHERE
        d.descriptor LIKE ?
      |]