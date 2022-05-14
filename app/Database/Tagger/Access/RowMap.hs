{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | Defines functions to map rows to types that do not have instances of FromRow.
module Database.Tagger.Access.RowMap
  ( reduceDbFwtList,
    descriptorOccurrenceMapParser,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as M
import qualified Data.Text as T
import Database.SQLite.Simple (Only (Only))
import qualified Database.SQLite.Simple.FromRow as FromRow
import Database.Tagger.Type
import Util.Core

type KeySet a = IntSet.IntSet

type DatabaseFWTMap = Map.Map FileKey (KeySet TagKey)

-- | Given a list of DatabaseFileWithTags,
-- folds all of the common files together to aggregate their tags keys.
-- Such that each tag key appears only once.
reduceDbFwtList :: [DatabaseFileWithTags] -> [DatabaseFileWithTags]
reduceDbFwtList = fromDbFwtMap . L.foldl' insertDbFwt Map.empty
  where
    insertDbFwt :: DatabaseFWTMap -> DatabaseFileWithTags -> DatabaseFWTMap
    insertDbFwt m f =
      maybe
        (Map.insert (fk' f) (tks' f) m)
        (\tks -> Map.insert (fk' f) (IntSet.union tks (tks' f)) m)
        (Map.lookup (fk' f) m)
      where
        fk' :: DatabaseFileWithTags -> FileKey
        fk' (TaggedFile_ fk _) = fk
        fk' (FileWithTags_ fk _) = fk
        tks' :: DatabaseFileWithTags -> KeySet TagKey
        tks' (TaggedFile_ _ tk) = IntSet.fromList . M.maybeToList $ tk
        tks' (FileWithTags_ _ tks) = IntSet.fromList tks
    fromDbFwtMap :: DatabaseFWTMap -> [DatabaseFileWithTags]
    fromDbFwtMap = map (uncurry FileWithTags_ . biFunctor IntSet.toList) . Map.toList
    biFunctor f (x, y) = (x, f y)

descriptorOccurrenceMapParser :: (Int, Int) -> OccurrenceMap Descriptor
descriptorOccurrenceMapParser (dk, c) = IntMap.singleton dk c