{-# LANGUAGE BangPatterns #-}
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
  find,
  Data.HierarchyMap.lookup,
  metaMember,
  infraMember,
  insert,
  empty,
  isMetaCircular,
  getAllInfraTo,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Hierarchy.Internal
import Data.Maybe

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
 Return empty set if the key is not in the map.
-}
find :: Hashable a => a -> HierarchyMap a -> HashSet.HashSet a
find x (HierarchyMap m) = fromMaybe HashSet.empty $ HashMap.lookup x m

{- |
 Return Nothing if the key is not in the map.
-}
lookup :: Hashable k => k -> HierarchyMap k -> Maybe (HashSet.HashSet k)
lookup x (HierarchyMap m) = HashMap.lookup x m

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
 Returns 'True` if the given value is meta to itself at any point in the map.
-}
isMetaCircular :: Hashable a => a -> HierarchyMap a -> Bool
isMetaCircular x = HashSet.member x . getAllInfraTo x

{- |
 Retrieve a set of all elements that are infra to the given value.

 Does not include the given value.
-}
getAllInfraTo :: Hashable a => a -> HierarchyMap a -> HashSet.HashSet a
getAllInfraTo x hm =
  let !layerInfra = Data.HierarchyMap.find x hm
   in HashSet.union layerInfra
        . HashSet.unions
        . map (`getAllInfraTo` hm)
        . HashSet.toList
        $ layerInfra
