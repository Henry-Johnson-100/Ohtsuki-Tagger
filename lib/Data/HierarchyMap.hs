{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

{- |
Module      : Data.HierarchyMap
Description : Definition of the HierarchyMap data type. A hierarchical structure of
  strict HashMaps

License     : GPL-3
Maintainer  : monawasensei@gmail.com

A 'HierarchyMap` is a newtype wrapped hashmap that encodes 'Meta' and 'Infra' relations
between its members. These relationships take on a tree structure, a 'HierarchyMap` is a
flat encoding of that tree structure.

For the purposes of this library, the 'HierarchyMap` is meant to be used to encode similar
structures found in the Tagger SQL databases.

Where a 'HierarchyMap` is roughly equivalent to any set of columns that have this definition:

@
CREATE TABLE META_RELATION (
meta INTEGER
infra INTEGER
FOREIGN KEY infra REFERENCES META_RELATION (meta)
);
@

Given that the tree of relations formed by this definition can be queried one layer at a time
or via the whole tree with a recursive CTE, the 'HierarchyMap` can be queried likewise.

A 'HierarchyMap` does not check for circular relationships, which would cause an inifite loop
upon evaluation. Therefore it is imperative that the relations inserted into a 'HierarchyMap`
are not circular. A 'HierarchyMap` does support multiple meta parents, and branched infra relations,
which makes it slightly more unique compared to some of the table definitions found in 'Database.Tagger.Script.schemaDefinition`
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
  isInfraTo,
  insert,
  inserts,
  empty,
  Data.HierarchyMap.null,
  getAllInfraTo,
  getAllMetaTo,
  keys,
) where

import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Hierarchy.Internal (
  HierarchyMap (..),
  empty,
  insert,
  unionWith,
 )
import Data.Maybe (fromMaybe)

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
 Insert a list of relation tuples.

 > flip $ foldl' (flip (uncurry insert))
-}
inserts ::
  (Hashable a, Foldable t) =>
  t (a, HashSet.HashSet a) ->
  HierarchyMap a ->
  HierarchyMap a
inserts = flip $ F.foldl' (flip (uncurry insert))

{- |
 'True` if the given value exists as either a meta or infra member.
-}
member :: Hashable a => a -> HierarchyMap a -> Bool
member x (HierarchyMap m) = HashMap.member x m

{- |
 'True` if the map is empty.
-}
null :: HierarchyMap k -> Bool
null (HierarchyMap m) = HashMap.null m

{- |
 Return empty set if the key is not in the map.
-}
find :: Hashable a => a -> HierarchyMap a -> HashSet.HashSet a
find x (HierarchyMap m) = fromMaybe HashSet.empty $ HashMap.lookup x m

{- |
 Return 'Nothing` if the key is not in the map.
-}
lookup :: Hashable k => k -> HierarchyMap k -> Maybe (HashSet.HashSet k)
lookup x (HierarchyMap m) = HashMap.lookup x m

{- |
 'True` if the given value has a meta relationship to any member of the map.

 The value must be a key in the map and have a non-null HashSet value.
-}
metaMember :: Hashable k => k -> HierarchyMap k -> Bool
metaMember x (HierarchyMap m) = maybe False (not . HashSet.null) (HashMap.lookup x m)

{- |
 'True` if the given value has an infra relationship to any member of the map.
-}
infraMember :: Hashable a => a -> HierarchyMap a -> Bool
infraMember x (HierarchyMap m) = maybe False HashSet.null $ HashMap.lookup x m

{- |
 'True` if the first given value is infra to the second in the given map.
-}
isInfraTo :: Hashable a => a -> a -> HierarchyMap a -> Bool
isInfraTo x y = HashSet.member x . getAllInfraTo y

{- |
 Retrieve a set of all values that are meta to the given value.

 Does not include the given value.
-}
getAllMetaTo :: Hashable a => a -> HierarchyMap a -> HashSet.HashSet a
getAllMetaTo x hm@(HierarchyMap m) =
  let !parentLayer = HashMap.keysSet $ HashMap.filter (HashSet.member x) m
   in HashSet.union parentLayer
        . HashSet.unions
        . map (`getAllMetaTo` hm)
        . HashSet.toList
        $ parentLayer

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

{- |
 Returns a list containing every member of the 'HierarchyMap`
-}
keys :: HierarchyMap k -> [k]
keys (HierarchyMap m) = HashMap.keys m