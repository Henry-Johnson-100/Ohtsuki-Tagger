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
  OccurrenceMapKey (..),
  get,
  set,
  occur,
  empty,
  union,
  unions,
) where

import Data.OccurrenceMap.Internal