module Data.OccurrenceHashMap (
  OccurrenceHashMap (..),
  get,
  set,
  occur,
  union,
  empty,
  unions,
  fromList,
) where

import Data.OccurrenceHashMap.Internal (
  OccurrenceHashMap (..),
  empty,
  fromList,
  get,
  occur,
  set,
  union,
  unions,
 )