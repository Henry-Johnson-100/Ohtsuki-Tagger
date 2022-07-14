{- |
Module      : Data.HierarchyTree
Description : Definition of the HierarchyTree data type. A hierarchical structure of
  hashable types that is injective with a HierarchyMap

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Data.HierarchyTree (
  HierarchyTree (..),
  relationNode,
  relationChildren,
  hierarchyTreeToMap,
  compareHierarchyTree,
) where

import Data.Hierarchy.Internal
import qualified Data.List.NonEmpty as NE

{- |
 Fetch the children trees of the current tree.
-}
relationChildren :: HierarchyTree a -> [HierarchyTree a]
relationChildren tr =
  case tr of
    Infra _ -> []
    Meta _ is -> NE.toList is

{- |
 Compare two 'HierarchyTree`s by comparing their nodes.
-}
compareHierarchyTree :: Ord a => HierarchyTree a -> HierarchyTree a -> Ordering
compareHierarchyTree trx try = compare (relationNode trx) (relationNode try)