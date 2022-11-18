{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Database.Tagger.Main (
  ) where

import qualified Data.HashSet as HS
import Database.Tagger
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL

basicQueryFunctionality :: IO TaggedConnection -> TestTree
basicQueryFunctionality conn =
  testGroup
    "Basic Queries"
    [ testCase "Pattern Wildcard" $ do
        r <- conn >>= taggerQL (TaggerQLQuery "p.%")
        a <- conn >>= allFiles
        assertEqual "p.% should retrieve all files in the database" (HS.fromList a) r
    , testCase "Subtag Search 0" $ do
        r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_5 {d.descriptor_6}")
        assertEqual
          "d.descriptor_5 {d.descriptor_6}"
          [File 4 "file_4", File 5 "file_5"]
          r
    ]

queryEdgeCases :: IO TaggedConnection -> TestTree
queryEdgeCases conn =
  testGroup
    "Query Edge Cases"
    [ testGroup
        "Tech-note f02a13240b for files, A: a{b{c}} and B: a{b} d{b{c}}"
        [ testCase
            "Return A for query a{b{c}}"
            $ do
              r <-
                conn
                  >>= taggerQL
                    (TaggerQLQuery "d.descriptor_8 {d.descriptor_9 {d.descriptor_10}}")
              assertEqual
                ""
                (HS.singleton $ File 6 "file_6")
                r
        , testCase "Return A, B for a{b}" $ do
            r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_8 {d.descriptor_9}}")
            assertEqual
              ""
              [File 6 "file_6", File 7 "file_7"]
              r
        , testCase "Return A, B for b{c}" $ do
            r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_9 {d.descriptor_10}")
            assertEqual
              ""
              [File 6 "file_6", File 7 "file_7"]
              r
        , testCase "Return B for d{b}" $ do
            r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_11 {d.descriptor_9}")
            assertEqual
              ""
              [File 7 "file_7"]
              r
        ]
    , testGroup
        "Ticket a50b7d8"
        [ testCase "Relational subqueries are not disjoint" $ do
            r <-
              conn
                >>= taggerQL
                  ( TaggerQLQuery
                      "r.descriptor_12 {d.descriptor_15 d.descriptor_16}"
                  )
            assertEqual
              ""
              [File 8 "file_8", File 9 "file_9"]
              r
        , testCase "Disjoint, single arity, relational subqueries are a superset" $ do
            r <-
              conn
                >>= taggerQL
                  ( TaggerQLQuery
                      "r.descriptor_12 {d.descriptor_15} \
                      \r.descriptor_12 {d.descriptor_16}"
                  )
            assertEqual
              ""
              [File 8 "file_8", File 9 "file_9", File 10 "file_10"]
              r
        , testCase "Descriptor subqueries are not disjoint" $ do
            r <-
              conn
                >>= taggerQL
                  ( TaggerQLQuery
                      "d.descriptor_13 {d.descriptor_15 d.descriptor_16}"
                  )
            assertEqual
              ""
              [File 8 "file_8"]
              r
        ]
    ]