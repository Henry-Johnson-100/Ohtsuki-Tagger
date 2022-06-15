{-# LANGUAGE StrictData #-}

{- |
Module      : Data.HierarchyMap
Description : Definition of the HashTree data type. A hierarchical structure of
  strict HashMaps

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Data.HierarchyMap (
  HierarchyMap,
  member,
  metaMember,
  infraMember,
  insert,
  empty,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)

{- |
 A flat 'HashMap` that encodes hierarchical relationships

 A \"meta\" relation means that some key
 'a` has a non-null HashSet of 'a` as it's value.

 An \"infra\" relation means that some key 'a` has a 'null` 'HashSet` of values.

 Both types of relations are accessable on one level of the 'HashMap`.

 Relations can be nested arbitrarily deep or defined circularly
 but are still represented
 as a single flat 'HashMap` in the underlying implementation.
-}
newtype HierarchyMap a
  = HierarchyMap (HashMap.HashMap a (HashSet.HashSet a))
  deriving (Show, Eq)

{- |
 True if the given value exists as either a meta or infra member.
-}
member :: Hashable a => a -> HierarchyMap a -> Bool
member x (HierarchyMap m) = HashMap.member x m

{- |
 True if the given value has a meta relationship to any member of the map.

 The value must be a key in the map and have a non-null HashSet value.
-}
metaMember :: Hashable k => k -> HierarchyMap k -> Bool
metaMember x (HierarchyMap m) = maybe False (not . HashSet.null) (HashMap.lookup x m)

{- |
 True if the given value has an infra relationship to any member of the map.
-}
infraMember :: Hashable a => a -> HierarchyMap a -> Bool
infraMember x (HierarchyMap m) = maybe False HashSet.null $ HashMap.lookup x m

{- |
 Inserts the given key or unions an existing entry's set if the key already exists.

 Adds empty k-v entries for each member of the given set, representing Infra relations.

 Does not prohibit circular relations.
-}
insert :: Hashable a => a -> HashSet.HashSet a -> HierarchyMap a -> HierarchyMap a
insert k s (HierarchyMap m) =
  let keyInserted = HierarchyMap $ HashMap.insertWith HashSet.union k s m
   in HashSet.foldl' (\hm k' -> insert k' HashSet.empty hm) keyInserted s

empty :: HierarchyMap a
empty = HierarchyMap HashMap.empty