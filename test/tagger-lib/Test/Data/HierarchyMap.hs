{-# LANGUAGE OverloadedLists #-}

module Test.Data.HierarchyMap (
    hierarchyMapTests,
) where

import qualified Data.HashSet as HS
import Data.HierarchyMap (
    HierarchyMap,
    empty,
    getAllInfraTo,
    getAllMetaTo,
    insert,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

hsint :: HS.HashSet Int -> HS.HashSet Int
hsint = id

hmint :: HierarchyMap Int -> HierarchyMap Int
hmint = id

hmSetOne :: HierarchyMap Int
hmSetOne =
    hmint
        . insert 1 [2, 3, 4, 5]
        . insert 3 [6, 7, 8, 9]
        $ empty

hmSetTwo :: HierarchyMap Int
hmSetTwo =
    hmint
        . insert 6 [10, 11]
        . insert 10 [12, 13, 14]
        . insert 12 [15, 16]
        $ hmSetOne

hierarchyMapTests :: TestTree
hierarchyMapTests =
    testGroup
        "HierarchyMap Tests"
        [ testCase
            "getAllInfraTo 1"
            ( assertEqual
                ""
                (hsint [2, 3, 4, 5, 6, 7, 8, 9])
                (getAllInfraTo 1 hmSetOne)
            )
        , testCase
            "getAllInfraTo 2"
            ( assertEqual
                ""
                (hsint [6, 7, 8, 9])
                (getAllInfraTo 3 hmSetOne)
            )
        , testCase
            "getAllInfraTo 3"
            ( let m = insert 3 [2] hmSetOne
               in assertEqual
                    (show m)
                    (hsint [6, 7, 8, 9, 2])
                    (getAllInfraTo 3 m)
            )
        , testCase
            "getAllMetaTo 1"
            ( assertEqual
                ""
                (hsint [3, 1])
                (getAllMetaTo 6 hmSetOne)
            )
        , testCase
            "getAllMetaTo 2"
            ( assertEqual
                ""
                (hsint [12, 10, 6, 3, 1])
                (getAllMetaTo 16 hmSetTwo)
            )
        , testCase
            "getAllMetaTo 3"
            ( assertEqual
                ""
                (hsint [12, 10, 6, 3, 4, 1])
                (getAllMetaTo 16 . insert 4 [16] $ hmSetTwo)
            )
        ]
