{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Hierarchy.Internal (
  module Data.Hierarchy.Internal,
) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Types.Injective

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
 'union` two 'HierarchyMap a`
-}
instance Hashable a => Semigroup (HierarchyMap a) where
  (<>) = unionWith HashSet.union

instance Hashable a => Monoid (HierarchyMap a) where
  mempty = empty

{- |
 Inserts the given key or unions an existing entry's set if the key already exists.

 Adds empty k-v entries for each member of the given set, representing Infra relations.

 Does not prohibit circular relations.

 Care should be taken that data inserted into the 'HierarchyMap` is not circularly related
 ex:

 > insert 1 [1,2] empty

 would created a map [(1, [1,2]), (2, [])] where 1 is circularly related to itself.
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

empty :: HierarchyMap a
empty = HierarchyMap HashMap.empty

{- |
 A non-flat data structure that encodes hierarchical relationships as a tree.

 Injective to a 'HierarchyMap` provided that its values are hashable.
-}
data HierarchyTree a
  = Infra a
  | Meta a (NE.NonEmpty (HierarchyTree a))
  deriving (Show, Eq, Functor, Foldable)

{- |
 > to = hierarchyTreeToMap
-}
instance Hashable a => Injective (HierarchyTree a) (HierarchyMap a) where
  to = hierarchyTreeToMap

{- |
 Fetch the top node of the current tree.
-}
relationNode :: HierarchyTree p -> p
relationNode tr =
  case tr of
    Infra x -> x
    Meta x _ -> x

{- |
 Inject a 'HierarchyTree` to a 'HierarchyMap`.
-}
hierarchyTreeToMap :: Hashable a => HierarchyTree a -> HierarchyMap a
hierarchyTreeToMap = hierarchyTreeToMap' empty
 where
  hierarchyTreeToMap' ::
    Hashable a =>
    HierarchyMap a ->
    HierarchyTree a ->
    HierarchyMap a
  hierarchyTreeToMap' acc tr =
    case tr of
      Infra x -> insert x HashSet.empty acc
      Meta x is ->
        unionWith
          HashSet.union
          (insert x (HashSet.fromList . NE.toList . NE.map relationNode $ is) acc)
          ( L.foldl1' (unionWith HashSet.union)
              . NE.toList
              . NE.map hierarchyTreeToMap
              $ is
          )

-- hierarchyKVToTree :: Hashable a => a -> HashSet.HashSet a -> HierarchyTree a
-- hierarchyKVToTree k vs =
--   if HashSet.null vs
--     then Infra k
--     else Meta