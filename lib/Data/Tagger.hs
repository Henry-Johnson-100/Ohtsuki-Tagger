{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Data.Tagger
Description : Common and miscellaneous types.

License     : GPL-3
Maintainer  : monawasensei@gmail.com

These types are multipurpose and are used in internal implementations
of tagger-lib as well as being present in the GUI or internal implementations
of tagger.
-}
module Data.Tagger (
  CyclicEnum (..),
  QueryCriteria (..),
  SetOp (..),
) where

import Data.Tagger.Internal (
  CyclicEnum (..),
  PatternableCriteria (..),
  UnPatternableCriteria (..),
 )

{- |
 A type detailing how set-like collections are to be combined.
-}
data SetOp
  = Union
  | Intersect
  | Difference
  deriving (Show, Eq, Bounded, Enum, Ord, CyclicEnum)

data QueryCriteria
  = -- | 'QueryCriteria` that correspond to attributes of records in the Tagger database
    -- that have some sort of data representation, usually text.
    TagCriteria PatternableCriteria
  | -- | 'QueryCriteria` that correspond to rows or entities in the Tagger database that
    -- have no data representation or are characterized by lack of data.
    OtherCriteria UnPatternableCriteria
  deriving (Show, Eq)

instance CyclicEnum QueryCriteria

instance Ord QueryCriteria where
  compare qcx qcy = compare (fromEnum qcx) (fromEnum qcy)

instance Enum QueryCriteria where
  toEnum n =
    case n of
      0 -> TagCriteria DescriptorCriteria
      1 -> TagCriteria MetaDescriptorCriteria
      2 -> TagCriteria FilePatternCriteria
      3 -> OtherCriteria UntaggedCriteria
      _ -> error "Out of bounds in toEnum :: Int -> QueryCriteria"
  fromEnum c =
    case c of
      TagCriteria pc ->
        case pc of
          DescriptorCriteria -> 0
          MetaDescriptorCriteria -> 1
          FilePatternCriteria -> 2
      OtherCriteria _ -> 3

instance Bounded QueryCriteria where
  minBound = TagCriteria DescriptorCriteria
  maxBound = OtherCriteria UntaggedCriteria