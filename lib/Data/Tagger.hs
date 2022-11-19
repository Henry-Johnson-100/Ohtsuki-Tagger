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
  SetOp (..),
) where

{- |
 Typeclass defining a bounded, ordered, cyclic enumeration.

 The methods 'next` and 'prev` are wrappers for 'succ` and 'pred`
 that wrap to either 'minBound` or 'maxBound`.
-}
class (Bounded e, Ord e, Enum e) => CyclicEnum e where
  next :: e -> e
  next x = if x >= maxBound then minBound else succ x
  prev :: e -> e
  prev x = if x <= minBound then maxBound else pred x

{- |
 A type detailing how set-like collections are to be combined.
-}
data SetOp
  = Union
  | Intersect
  | Difference
  deriving (Show, Eq, Bounded, Enum, Ord, CyclicEnum)