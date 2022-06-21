{-# LANGUAGE DeriveAnyClass #-}

module Data.Tagger (
  CyclicEnum (..),
  QueryCriteria (..),
  SetOp (..),
) where

class (Bounded e, Ord e, Enum e) => CyclicEnum e where
  next :: e -> e
  next x = if x >= maxBound then minBound else succ x
  prev :: e -> e
  prev x = if x <= minBound then maxBound else pred x

data QueryCriteria
  = DescriptorCriteria
  | MetaDescriptorCriteria
  | FilePatternCriteria
  | UntaggedCriteria
  deriving (Show, Eq, Bounded, Enum, Ord, CyclicEnum)

data SetOp
  = Union
  | Intersect
  | Difference
  deriving (Show, Eq, Bounded, Enum, Ord, CyclicEnum)