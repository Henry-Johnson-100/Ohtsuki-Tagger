{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test.Text.TaggerQL.Expression.Engine (
  queryEngineASTTests,
) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashSet as HS
import Data.Tagger
import qualified Data.Text as T
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
    , queryEdgeCases c
    , taggingEngineTests c
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
        r <-
          c
            >>= runExpr
              ( Binary
                  (FileTermValue "%")
                  Difference
                  (TagTermValue (DescriptorTerm "%"))
              )
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
          [file 8, file 9, file 10]
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
              [file 1, file 2]
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
              [file 4, file 5]
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
              [file 3, file 4, file 5]
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
              [file 1, file 3]
              r
        ]
    , testGroup
        "TagExpressions"
        [ testCase "Simple TagExpression" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_5")
                      (SubTag (DescriptorTerm "descriptor_6"))
                  )
            assertEqual
              "Simple subtag 4{5} should find files with 4{5} tags."
              [file 4, file 5]
              r
        , testCase "Flat SubTag TagExpression" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_6")
                      (SubTag (DescriptorTerm "descriptor_7"))
                  )
            assertEqual
              "SubTag queries are a flat operation."
              [file 5]
              r
        , testCase "Complex Nested SubTag" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_17")
                      ( SubExpression
                          (DescriptorTerm "descriptor_18")
                          (SubTag (DescriptorTerm "descriptor_20"))
                      )
                  )
            assertEqual
              "SubExpressions modify the supertag environment for lower depths."
              [file 15]
              r
        , testCase "SubTag Expression - 1" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_17")
                      (SubTag (DescriptorTerm "descriptor_18"))
                  )
            assertEqual
              "The LHS of the below test."
              [ file 11
              , file 13
              , file 15
              , file 16
              ]
              r
        , testCase "SubExpression Tags - 0" $ do
            h <- c >>= queryTags (DescriptorTerm "descriptor_17")
            r <-
              c
                >>= runReaderT
                  ( evalSubExpression
                      (SubTag (DescriptorTerm "descriptor_18"))
                      (HS.fromList h)
                  )
            assertEqual
              "Matches the tags returned by the LHS of the \
              \\"TagExpressions - SubBinary Sub Union\" test."
              [ Tag 28 11 17 Nothing
              , Tag 32 13 17 Nothing
              , Tag 38 15 17 Nothing
              , Tag 41 16 17 Nothing
              ]
              r
        , testCase "SubExpression Tags - 1" $ do
            h <- c >>= queryTags (DescriptorTerm "descriptor_17")
            r <-
              c
                >>= runReaderT
                  ( evalSubExpression
                      (SubTag (DescriptorTerm "descriptor_19"))
                      (HS.fromList h)
                  )
            assertEqual
              "Matches the tags returned by the RHS of the \
              \\"TagExpressions - SubBinary Sub Union\" test."
              [ Tag 30 12 17 Nothing
              , Tag 32 13 17 Nothing
              ]
              r
        , testGroup
            "TagExpressions - SubBinary"
            [ testCase "Sub Union" $ do
                r <-
                  c
                    >>= runExpr
                      ( TagExpression
                          (DescriptorTerm "descriptor_17")
                          ( SubBinary
                              (SubTag (DescriptorTerm "descriptor_18"))
                              Union
                              (SubTag (DescriptorTerm "descriptor_19"))
                          )
                      )
                assertEqual
                  "SubUnion filters supertags if the supertag\
                  \ is subtagged by either one or the other subtag sets."
                  [ file 11
                  , file 12
                  , file 13
                  , file 15
                  , file 16
                  ]
                  r
            , testCase "Sub Intersection" $ do
                r <-
                  c
                    >>= runExpr
                      ( TagExpression
                          (DescriptorTerm "descriptor_17")
                          ( SubBinary
                              (SubTag (DescriptorTerm "descriptor_18"))
                              Intersect
                              (SubTag (DescriptorTerm "descriptor_19"))
                          )
                      )
                assertEqual
                  "SubUnion filters supertags if the supertag\
                  \ is a member of both subtag sets."
                  [ file 13
                  ]
                  r
            , testCase "Sub Difference" $ do
                r <-
                  c
                    >>= runExpr
                      ( TagExpression
                          (DescriptorTerm "descriptor_17")
                          ( SubBinary
                              (SubTag (DescriptorTerm "descriptor_18"))
                              Difference
                              (SubTag (DescriptorTerm "descriptor_19"))
                          )
                      )
                assertEqual
                  "SubUnion filters supertags if the supertag\
                  \ is a member of the first and not the second subtag set."
                  [ file 11
                  , file 15
                  , file 16
                  ]
                  r
            ]
        ]
    , testGroup
        "Misc queries"
        [ testCase "Descriptor Wildcard and SubExpressions - 0" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "%")
                      ( SubBinary
                          (SubTag (DescriptorTerm "descriptor_20"))
                          Difference
                          (SubTag (DescriptorTerm "descriptor_18"))
                      )
                  )
            assertEqual
              ""
              [ file 14
              , file 15
              -- , file 16 This file is removed by difference
              ]
              r
        ]
    ]

queryEdgeCases :: IO TaggedConnection -> TestTree
queryEdgeCases c =
  testGroup
    "Query Engine AST - Edge Cases"
    [ testGroup
        "Tech-note f02a13240b for files, A: a{b{c}} and B: a{b} d{b{c}}"
        [ testCase "Return A for query a{b{c}}" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_8")
                      ( SubExpression
                          (DescriptorTerm "descriptor_9")
                          (SubTag (DescriptorTerm "descriptor_10"))
                      )
                  )
            assertEqual
              ""
              [file 6]
              r
        , testCase "Return A, B for a{b}" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_8")
                      (SubTag (DescriptorTerm "descriptor_9"))
                  )
            assertEqual
              ""
              [file 6, file 7]
              r
        , testCase "Return A, B for b{c}" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_9")
                      (SubTag (DescriptorTerm "descriptor_10"))
                  )
            assertEqual
              ""
              [file 6, file 7]
              r
        , testCase "Return B for d{b}" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_11")
                      (SubTag (DescriptorTerm "descriptor_9"))
                  )
            assertEqual
              ""
              [file 7]
              r
        ]
    , testGroup
        "Ticket a50b7d8"
        [ testCase "Relational subqueries are not disjoint" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (MetaDescriptorTerm "descriptor_12")
                      ( SubBinary
                          (SubTag (DescriptorTerm "descriptor_15"))
                          Intersect
                          (SubTag (DescriptorTerm "descriptor_16"))
                      )
                  )
            assertEqual
              ""
              [file 8, file 9]
              r
        , testCase "Disjoint, single arity, relational subqueries are a superset" $ do
            r <-
              c
                >>= runExpr
                  ( Binary
                      ( TagExpression
                          (MetaDescriptorTerm "descriptor_12")
                          (SubTag (DescriptorTerm "descriptor_15"))
                      )
                      Intersect
                      ( TagExpression
                          (MetaDescriptorTerm "descriptor_12")
                          (SubTag (DescriptorTerm "descriptor_16"))
                      )
                  )
            assertEqual
              ""
              [file 8, file 9, file 10]
              r
        , testCase "Descriptor subqueries are not disjoint" $ do
            r <-
              c
                >>= runExpr
                  ( TagExpression
                      (DescriptorTerm "descriptor_13")
                      ( SubBinary
                          (SubTag (DescriptorTerm "descriptor_15"))
                          Intersect
                          (SubTag (DescriptorTerm "descriptor_16"))
                      )
                  )
            assertEqual
              ""
              [file 8]
              r
        ]
    ]

taggingEngineTests :: IO TaggedConnection -> TestTree
taggingEngineTests c =
  testGroup
    "Tagging Engine Tests"
    [ testCase "Tagging Engine - 0" $ do
        let se =
              SubExpression
                (td 21)
                ( SubBinary
                    (SubTag (td 22))
                    Intersect
                    (SubTag (td 23))
                )
            fk = 17
            expectedResults =
              [ Tag 44 17 21 Nothing
              , Tag 45 17 22 (Just 44)
              , Tag 46 17 23 (Just 44)
              ]
        c
          >>= runSubExprOnFile
            se
            fk
        f <- fmap taggedFileTags <$> (c >>= runMaybeT . queryForTaggedFileWithFileId fk)
        assertEqual
          ""
          ( Just
              expectedResults
          )
          f
    , after AllSucceed "Tagging Engine - 0" $
        testCase "Tagging Engine - 1" $ do
          let se =
                SubExpression
                  (td 21)
                  ( SubBinary
                      ( SubExpression
                          (td 22)
                          ( SubBinary
                              (SubTag (td 24))
                              Intersect
                              (SubTag (td 25))
                          )
                      )
                      Intersect
                      (SubTag (td 23))
                  )
              fk = 17
              expectedResults =
                [ Tag 44 17 21 Nothing
                , Tag 45 17 22 (Just 44)
                , Tag 46 17 23 (Just 44)
                , Tag 47 17 24 (Just 45)
                , Tag 48 17 25 (Just 45)
                ]
          c
            >>= runSubExprOnFile
              se
              fk
          f <- fmap taggedFileTags <$> (c >>= runMaybeT . queryForTaggedFileWithFileId fk)
          assertEqual
            ""
            ( Just
                expectedResults
            )
            f
    , after AllSucceed "Tagging Engine - 1" $
        testCase "Tagging Engine - 2" $ do
          let se =
                SubExpression
                  (td 26)
                  ( SubBinary
                      ( SubExpression
                          (td 27)
                          ( SubBinary
                              ( SubExpression
                                  (td 28)
                                  (SubTag (td 29))
                              )
                              Intersect
                              (SubTag (td 30))
                          )
                      )
                      Intersect
                      (SubTag (td 31))
                  )
              fk = 18
              expectedResults =
                [ Tag 49 18 26 Nothing
                , Tag 50 18 27 (Just 49)
                , Tag 51 18 28 (Just 50)
                , Tag 52 18 29 (Just 51)
                , Tag 53 18 30 (Just 50)
                , Tag 54 18 31 (Just 49)
                ]
          c
            >>= runSubExprOnFile
              se
              fk
          f <- fmap taggedFileTags <$> (c >>= runMaybeT . queryForTaggedFileWithFileId fk)
          assertEqual
            ""
            ( Just
                expectedResults
            )
            f
    , after AllSucceed "Tagging Engine - 2" . testCase "Tagging Engine - 3" $ do
        let se =
              SubBinary
                ( SubBinary
                    (SubTag (td 32))
                    Intersect
                    (SubTag (td 33))
                )
                Intersect
                (SubTag (td 34))
            fk = 19
            expectedResults =
              [ Tag 55 19 32 Nothing
              , Tag 56 19 33 Nothing
              , Tag 57 19 34 Nothing
              ]
        c
          >>= runSubExprOnFile
            se
            fk
        f <- fmap taggedFileTags <$> (c >>= runMaybeT . queryForTaggedFileWithFileId fk)
        assertEqual
          ""
          ( Just
              expectedResults
          )
          f
    , after AllSucceed "Tagging Engine - 3" . testCase "Tagging Engine - 4" $ do
        let se =
              SubExpression (td 33) (SubTag (td 32))
            fk = 19
            expectedResults =
              [ Tag 55 19 32 Nothing
              , Tag 56 19 33 Nothing
              , Tag 57 19 34 Nothing
              , Tag 58 19 32 (Just 56)
              ]
        c
          >>= runSubExprOnFile
            se
            fk
        f <- fmap taggedFileTags <$> (c >>= runMaybeT . queryForTaggedFileWithFileId fk)
        assertEqual
          ""
          ( Just
              expectedResults
          )
          f
    ]

file :: RecordKey File -> File
file n = File n ("file_" <> (T.pack . show $ n))

td :: RecordKey Descriptor -> TagTerm
td n = MetaDescriptorTerm ("descriptor_" <> (T.pack . show $ n))