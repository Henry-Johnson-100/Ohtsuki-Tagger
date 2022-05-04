{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Defines functions to map rows to types that do not have instances of FromRow.
module Database.Tagger.Access.RowMap
  ( fileWithTagsMapper,
    fileWithMaybeTagsMapper,
    tagCountMapper,
  )
where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.SQLite.Simple (Only (Only))
import Database.Tagger.Type
  ( Descriptor (Descriptor),
    File (File),
    FileWithTags (FileWithTags, tags),
    TagCount,
    fwtFileEqual,
    pushTag,
  )

fileWithTagsMapper :: [(Int, T.Text, Int, T.Text)] -> [FileWithTags]
fileWithTagsMapper =
  fileWithMaybeTagsMapper
    . map (\(fid, fp, dids, dds) -> (fid, fp, M.Just dids, M.Just dds))

fileWithMaybeTagsMapper :: [(Int, T.Text, Maybe Int, Maybe T.Text)] -> [FileWithTags]
fileWithMaybeTagsMapper = fileWithMaybeTagsMapper' []

fileWithMaybeTagsMapper' ::
  [FileWithTags] ->
  [(Int, T.Text, Maybe Int, Maybe T.Text)] ->
  [FileWithTags]
fileWithMaybeTagsMapper' accum [] = accum
fileWithMaybeTagsMapper' accum (r : rs) =
  case accum of
    [] -> fileWithMaybeTagsMapper' [fileWithMaybeTagMapper' r] rs
    (old : past) ->
      let !new = fileWithMaybeTagMapper' r
       in if new `fwtFileEqual` old
            then fileWithMaybeTagsMapper' (L.foldl' pushTag old (tags new) : past) rs
            else fileWithMaybeTagsMapper' (new : old : past) rs
  where
    fileWithMaybeTagMapper' :: (Int, T.Text, Maybe Int, Maybe T.Text) -> FileWithTags
    fileWithMaybeTagMapper' (fid, fp, dids, dds) =
      FileWithTags
        (File fid fp)
        (maybe [] (\d -> [Descriptor (M.fromJust dids) d]) dds)

tagCountMapper :: Descriptor -> Only Int -> TagCount
tagCountMapper d (Only n) = (d, n)
