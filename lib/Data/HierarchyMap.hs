{-# LANGUAGE StrictData #-}

{- |
Module      : Data.HierarchyMap
Description : Definition of the HierarchyMap data type. A hierarchical structure of
  strict HashMaps

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Data.HierarchyMap (
  HierarchyMap,
  mapHierarchyMap,
  union,
  member,
  metaMember,
  infraMember,
  insert,
  empty,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Hierarchy.Internal

{- |
 Map over all elements in the hierarchy.
-}
mapHierarchyMap :: Hashable a => (k1 -> a) -> HierarchyMap k1 -> HierarchyMap a
mapHierarchyMap f (HierarchyMap m) =
  HierarchyMap . HashMap.mapKeys f . HashMap.map (HashSet.map f) $ m

{- |
 Union two 'HierarchyMap a` together. Combining the infra relations
 for any overlapping keys.
-}
union :: Hashable a => HierarchyMap a -> HierarchyMap a -> HierarchyMap a
union = unionWith HashSet.union

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
