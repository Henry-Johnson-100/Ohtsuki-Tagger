{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.OccurrenceMap.Internal (
  module Data.OccurrenceMap.Internal,
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import Data.Maybe (fromMaybe)

{- |
 An 'IntMap` with some specialized functions. This type is usually used for
 counting how many times something occurs, in the case of YuiTagger, it's for counting
 how many times a 'Tag` occurs.
-}
newtype OccurrenceMap = OccurrenceMap {occurrenceMap :: IntMap Int} deriving (Show, Eq)

{- |
 Get the Occurrences that correspond to the given Int.

 Returns 0 if it has not been incremented.
-}
get :: Integral k => k -> OccurrenceMap -> Int
get (fromIntegral -> k) = fromMaybe 0 . IntMap.lookup k . occurrenceMap

{- |
 For the first given Int, set its occurrences with the second.
-}
set :: Integral k => k -> Int -> OccurrenceMap -> OccurrenceMap
set (fromIntegral -> k) occurs (OccurrenceMap m) =
  OccurrenceMap $ IntMap.insert k occurs m

{- |
 Increment the location of the given Int.
-}
occur :: Integral k => k -> OccurrenceMap -> OccurrenceMap
occur (fromIntegral -> k) (OccurrenceMap m) =
  OccurrenceMap $ IntMap.insertWith (+) k 1 m

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

fromList :: Integral k => [(k, Int)] -> OccurrenceMap
fromList = OccurrenceMap . IntMap.fromList . map (first fromIntegral)
 where
  first f (x, y) = (f x, y)