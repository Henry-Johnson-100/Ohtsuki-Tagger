module Data.OccurrenceHashMap (
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

import Data.OccurrenceHashMap.Internal (
  OccurrenceHashMap (..),
  empty,
  fromList,
  get,
  occur,
  set,
  toList,
  union,
  unions,
 )