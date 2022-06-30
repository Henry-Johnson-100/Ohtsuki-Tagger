{- |
Module      : Data.OccurrenceMap
Description : Definition of the OccurrenceMap data type.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

An 'OccurrenceMap` is simply an 'IntMap` of Ints. It has functions getting, and
incrementing a key. An 'OccurrenceMap` holds every member of the set 'Int` and,
when queried for a certain member, will return 0 unless that member has been incremented.

These queries are called occurrences.
-}
module Data.OccurrenceMap (
  OccurrenceMap (..),
  get,
  set,
  occur,
  empty,
  union,
  unions,
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import Data.Maybe (fromMaybe)

newtype OccurrenceMap = OccurrenceMap {occurrenceMap :: IntMap Int} deriving (Show, Eq)

{- |
 Get the Occurrences that correspond to the given Int.

 Returns 0 if it has not been incremented.
-}
get :: Int -> OccurrenceMap -> Int
get k = fromMaybe 0 . IntMap.lookup k . occurrenceMap

{- |
 For the first given Int, set its occurrences with the second.
-}
set :: Int -> Int -> OccurrenceMap -> OccurrenceMap
set k occurs (OccurrenceMap m) = OccurrenceMap $ IntMap.insert k occurs m

{- |
 Increment the location of the given Int.
-}
occur :: Int -> OccurrenceMap -> OccurrenceMap
occur k (OccurrenceMap m) = OccurrenceMap $ IntMap.insertWith (+) k 1 m

{- |
 Combine two Maps, summing all of their locations.
-}
union :: OccurrenceMap -> OccurrenceMap -> OccurrenceMap
union (OccurrenceMap x) (OccurrenceMap y) =
  OccurrenceMap $ IntMap.unionWith (+) x y

{- |
 Return an initialized Map where every location has 0 occurrences.
-}
empty :: OccurrenceMap
empty = OccurrenceMap IntMap.empty

{- |
 Fold of many unions.
-}
unions :: [OccurrenceMap] -> OccurrenceMap
unions [] = empty
unions xs = L.foldl1' union xs