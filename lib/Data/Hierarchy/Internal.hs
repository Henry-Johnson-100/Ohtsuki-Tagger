{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Hierarchy.Internal (
  module Data.Hierarchy.Internal,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)

{- |
 A flat 'HashMap` that encodes hierarchical relationships

 A \"meta\" relation means that some key
 'a` has a non-null HashSet of 'a` as it's value.

 An \"infra\" relation means that some key 'a`
 is contained in the hashset of some other key in the map.

 Both types of relations are accessable on one level of the 'HashMap`.

 Relations can be nested arbitrarily deep
 but are still represented
 as a single flat 'HashMap` in the underlying implementation.
-}
newtype HierarchyMap a
  = HierarchyMap (HashMap.HashMap a (HashSet.HashSet a))
  deriving (Show, Eq)

{- |
 'union` two 'HierarchyMap a`
-}
instance Hashable a => Semigroup (HierarchyMap a) where
  (<>) :: Hashable a => HierarchyMap a -> HierarchyMap a -> HierarchyMap a
  (<>) = unionWith HashSet.union

instance Hashable a => Monoid (HierarchyMap a) where
  mempty :: Hashable a => HierarchyMap a
  mempty = empty

{- |
 Inserts the given key or unions an existing entry's set if the key already exists.

 Adds empty k-v entries for each member of the given set, representing Infra relations.

 Does not prohibit circular relations.

 Care should be taken that data inserted into the 'HierarchyMap` is not circularly related
 ex:

 > insert 1 (fromList [1,2]) empty

 will create a map @[(1, [1,2]), (2, [])]@ where 1 is circularly related to itself.
 This would cause an infinite hang if ever called.
-}
insert :: Hashable a => a -> HashSet.HashSet a -> HierarchyMap a -> HierarchyMap a
insert k s (HierarchyMap m) =
  let keyInserted = HierarchyMap $ HashMap.insertWith HashSet.union k s m
   in HashSet.foldl' (\hm k' -> insert k' HashSet.empty hm) keyInserted s

unionWith ::
  Hashable a =>
  (HashSet.HashSet a -> HashSet.HashSet a -> HashSet.HashSet a) ->
  HierarchyMap a ->
  HierarchyMap a ->
  HierarchyMap a
unionWith f (HierarchyMap x) (HierarchyMap y) = HierarchyMap $ HashMap.unionWith f x y

{- |
 Returns empty map.
-}
empty :: HierarchyMap a
empty = HierarchyMap HashMap.empty
