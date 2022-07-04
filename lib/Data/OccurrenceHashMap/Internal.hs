module Data.OccurrenceHashMap.Internal (
  OccurrenceHashMap (..),
  get,
  set,
  occur,
  union,
  empty,
  unions,
  fromList,
  toList,
) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Maybe (fromMaybe)

{- |
 The same as 'OccurrenceMap` except it stores a 'Hashable` value as a key
 instead of just an Int. Has the same interface as 'OccurrenceMap` so see that
 for function documentation.
-}
newtype OccurrenceHashMap a = OccurrenceHashMap {occurrenceHashMap :: HashMap a Int}
  deriving (Show, Eq)

get :: Hashable k => k -> OccurrenceHashMap k -> Int
get x = fromMaybe 0 . HashMap.lookup x . occurrenceHashMap

set :: Hashable k => k -> Int -> OccurrenceHashMap k -> OccurrenceHashMap k
set k n (OccurrenceHashMap m) = OccurrenceHashMap $ HashMap.insert k n m

occur :: Hashable k => k -> OccurrenceHashMap k -> OccurrenceHashMap k
occur k (OccurrenceHashMap m) = OccurrenceHashMap $ HashMap.insertWith (+) k 1 m

union ::
  Hashable a =>
  OccurrenceHashMap a ->
  OccurrenceHashMap a ->
  OccurrenceHashMap a
union (OccurrenceHashMap x) (OccurrenceHashMap y) =
  OccurrenceHashMap $ HashMap.unionWith (+) x y

empty :: OccurrenceHashMap a
empty = OccurrenceHashMap HashMap.empty

unions :: Hashable a => [OccurrenceHashMap a] -> OccurrenceHashMap a
unions [] = empty
unions xs = L.foldl1' union xs

fromList :: Hashable k => [(k, Int)] -> OccurrenceHashMap k
fromList = OccurrenceHashMap . HashMap.fromList

toList :: OccurrenceHashMap k -> [(k, Int)]
toList = HashMap.toList . occurrenceHashMap