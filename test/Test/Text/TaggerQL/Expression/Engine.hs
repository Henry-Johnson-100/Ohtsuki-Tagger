{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Text.TaggerQL.Expression.Engine (
  queryEngineASTTests,
) where

import qualified Data.HashSet as HS
import Data.Tagger
import Database.Tagger
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Engine

queryEngineASTTests :: IO TaggedConnection -> TestTree
queryEngineASTTests c =
  testGroup
    "Query Engine AST Tests"
    [ basicQueryFunctionality c
    ]

basicQueryFunctionality :: IO TaggedConnection -> TestTree
basicQueryFunctionality c =
  testGroup
    "Query Engine AST Tests - Basic"
    [ testCase "Pattern Wildcard" $ do
        r <- c >>= runExpr (FileTermValue "%")
        a <- c >>= allFiles
        assertEqual
          "FileTermValue \"%\" matches all files"
          (HS.fromList a)
          r
    , testCase "Untagged Files" $ do
        r <- c >>= runExpr UntaggedConst
        a <- c >>= queryForUntaggedFiles
        assertEqual
          "Untagged Constant"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - Descriptor" $ do
        r <- c >>= runExpr (TagTermValue (DescriptorTerm "descriptor_5"))
        a <- c >>= flatQueryForFileByTagDescriptorPattern "descriptor_5"
        assertEqual
          "A TagTermValue performs a flat query"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - Descriptor - Flat Query" $ do
        r <- c >>= runExpr (TagTermValue (DescriptorTerm "descriptor_6"))
        a <- c >>= flatQueryForFileByTagDescriptorPattern "descriptor_6"
        assertEqual
          "A TagTermValue performs a flat query"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - MetaDescriptor" $ do
        r <- c >>= runExpr (TagTermValue (MetaDescriptorTerm "descriptor_12"))
        a <- c >>= flatQueryForFileOnMetaRelationPattern "descriptor_12"
        assertEqual
          "A TagTermValue performs a flat query"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - MetaDescriptor - Manual" $ do
        r <- c >>= runExpr (TagTermValue (MetaDescriptorTerm "descriptor_12"))
        assertEqual
          "Should match the case: \"TagTermValue Expression - MetaDescriptor\""
          [File 8 "file_8", File 9 "file_9", File 10 "file_10"]
          r
    , testGroup
        "Binary Expressions"
        [ testCase "Union" $ do
            r <-
              c
                >>= runExpr
                  ( Binary
                      (FileTermValue "file_1")
                      Union
                      (FileTermValue "file_2")
                  )
            assertEqual
              "Union wa union dayo"
              [File 1 "file_1", File 2 "file_2"]
              r
        , testCase "Intersect - Simple Operands" $ do
            r <-
              c
                >>= runExpr
                  ( Binary
                      (TagTermValue (DescriptorTerm "descriptor_5"))
                      Intersect
                      (TagTermValue (DescriptorTerm "descriptor_6"))
                  )
            assertEqual
              ""
              [File 4 "file_4", File 5 "file_5"]
              r
        , testCase "Intersect - Complex Operand" $ do
            r <-
              c
                >>= runExpr
                  ( Binary
                      ( Binary
                          (TagTermValue (DescriptorTerm "descriptor_5"))
                          Union
                          (FileTermValue "file_3")
                      )
                      Intersect
                      (TagTermValue (DescriptorTerm "descriptor_6"))
                  )
            assertEqual
              "Binary Operations should be nestable."
              [File 3 "file_3", File 4 "file_4", File 5 "file_5"]
              r
        , testCase "Difference" $ do
            r <-
              c
                >>= runExpr
                  ( Binary
                      ( Binary
                          (TagTermValue (DescriptorTerm "descriptor_4"))
                          Union
                          ( Binary
                              (TagTermValue (DescriptorTerm "descriptor_5"))
                              Union
                              (TagTermValue (DescriptorTerm "descriptor_6"))
                          )
                      )
                      Difference
                      (TagTermValue (DescriptorTerm "descriptor_5"))
                  )
            assertEqual
              "Difference wa difference dayo"
              [File 1 "file_1", File 3 "file_3"]
              r
        ]
    , testGroup
        "TagExpressions"
        []
    ]