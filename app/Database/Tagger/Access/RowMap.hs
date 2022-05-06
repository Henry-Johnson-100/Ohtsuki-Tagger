{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Defines functions to map rows to types that do not have instances of FromRow.
module Database.Tagger.Access.RowMap
  ( reduceDbFwtList,
    descriptorOccurrenceMapParser,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.SQLite.Simple (Only (Only))
import qualified Database.SQLite.Simple.FromRow as FromRow
import Database.Tagger.Type
import Util.Core

type DatabaseFWTMap = Map.Map FileKey [TagKey]

-- | Given a list of DatabaseFileWithTags,
-- folds all of the common files together to aggregate their tags keys.
reduceDbFwtList :: [DatabaseFileWithTags] -> [DatabaseFileWithTags]
reduceDbFwtList = fromDbFwtMap . L.foldl' insertDbFwt Map.empty
  where
    insertDbFwt :: DatabaseFWTMap -> DatabaseFileWithTags -> DatabaseFWTMap
    insertDbFwt m f =
      maybe
        (Map.insert (fk' f) (tks' f) m)
        (\tks -> Map.insert (fk' f) (tks ++ tks' f) m)
        (Map.lookup (fk' f) m)
      where
        fk' (TaggedFile_ fk _) = fk
        fk' (FileWithTags_ fk _) = fk
        tks' (TaggedFile_ _ tk) = M.maybeToList tk
        tks' (FileWithTags_ _ tks) = tks
    fromDbFwtMap :: DatabaseFWTMap -> [DatabaseFileWithTags]
    fromDbFwtMap = map (uncurry FileWithTags_) . Map.toList

descriptorOccurrenceMapParser :: (Int, Int) -> OccurrenceMap Descriptor
descriptorOccurrenceMapParser (dk, c) = IntMap.singleton dk c