{-# HLINT ignore "Use const" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Text.TaggerQL.Expression.Engine (
  queryEngineASTTests,
) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust, isJust)
import Data.Tagger
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Engine

queryEngineASTTests :: IO TaggedConnection -> TestTree
queryEngineASTTests c =
  testGroup
    "Query Engine AST Tests"
    [ basicQueryFunctionality c
    , queryExpressionBasicFunctionality c
    , queryEdgeCases c
    , queryExpressionEdgeCases c
    , taggingEngineTests c
    , enginePropertyTests
    ]

fe :: Pattern -> QueryExpression
fe = QueryExpression . pure . FileLeaf

tle :: TagExpression (DTerm Pattern) -> QueryExpression
tle = QueryExpression . pure . TagLeaf

tedp :: DTerm Pattern -> TagExpression (DTerm Pattern)
tedp = pure

d :: a -> DTerm a
d = DTerm

rt :: a -> DTerm a
rt = DMetaTerm

des :: Int -> Pattern
des n = PatternText $ "descriptor_" <> (T.pack . show $ n)

fil :: Int -> Pattern
fil n = PatternText $ "file_" <> (T.pack . show $ n)

queryExpressionEdgeCases :: IO TaggedConnection -> TestTree
queryExpressionEdgeCases c =
  let qqe = flip queryQueryExpression
   in testGroup
        "Query Engine AST - Edge Cases"
        [ testGroup
            "Tech-note f02a13240b for files, A: a{b{c}} and B: a{b} d{b{c}}"
            [ testCase "Return A for query a{b{c}}" $ do
                r <-
                  c
                    >>= qqe
                      ( tle
                          ( (tedp . d . des $ 8)
                              # ( (tedp . d . des $ 9)
                                    # (tedp . d . des $ 10)
                                )
                          )
                      )
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_8")
                --       ( SubExpression $
                --           TagTermExtension
                --             (DescriptorTerm "descriptor_9")
                --             (SubTag (DescriptorTerm "descriptor_10"))
                --       )
                -- )
                assertEqual
                  ""
                  [file 6]
                  r
            , testCase "Return A, B for a{b}" $ do
                r <-
                  c
                    >>= qqe
                      (tle ((tedp . d . des $ 8) # (tedp . d . des $ 9)))
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_8")
                --       (SubTag (DescriptorTerm "descriptor_9"))
                -- )
                assertEqual
                  ""
                  [file 6, file 7]
                  r
            , testCase "Return A, B for b{c}" $ do
                r <-
                  c
                    >>= qqe
                      (tle ((tedp . d . des $ 9) # (tedp . d . des $ 10)))
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_9")
                --       (SubTag (DescriptorTerm "descriptor_10"))
                -- )
                assertEqual
                  ""
                  [file 6, file 7]
                  r
            , testCase "Return B for d{b}" $ do
                r <-
                  c
                    >>= qqe
                      (tle ((tedp . d . des $ 11) # (tedp . d . des $ 9)))
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_11")
                --       (SubTag (DescriptorTerm "descriptor_9"))
                -- )
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
                    >>= qqe
                      ( tle
                          ( (tedp . rt . des $ 12)
                              # ( (tedp . d . des $ 15)
                                    *. (tedp . d . des $ 16)
                                )
                          )
                      )
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (MetaDescriptorTerm "descriptor_12")
                --       ( BinarySubExpression $
                --           BinaryOperation
                --             (SubTag (DescriptorTerm "descriptor_15"))
                --             Intersect
                --             (SubTag (DescriptorTerm "descriptor_16"))
                --       )
                -- )
                assertEqual
                  ""
                  [file 8, file 9]
                  r
            , testCase "Disjoint, single arity, relational subqueries are a superset" $ do
                r <-
                  c
                    >>= qqe
                      ( tle ((tedp . rt . des $ 12) # (tedp . d . des $ 15))
                          *. tle ((tedp . rt . des $ 12) # (tedp . d . des $ 16))
                      )
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       ( TagExpression $
                --           TagTermExtension
                --             (MetaDescriptorTerm "descriptor_12")
                --             (SubTag (DescriptorTerm "descriptor_15"))
                --       )
                --       Intersect
                --       ( TagExpression $
                --           TagTermExtension
                --             (MetaDescriptorTerm "descriptor_12")
                --             (SubTag (DescriptorTerm "descriptor_16"))
                --       )
                -- )
                assertEqual
                  ""
                  [file 8, file 9, file 10]
                  r
            , testCase "Descriptor subqueries are not disjoint" $ do
                r <-
                  c
                    >>= qqe
                      ( tle
                          ( (tedp . d . des $ 13)
                              # ( (tedp . d . des $ 15)
                                    *. (tedp . d . des $ 16)
                                )
                          )
                      )
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_13")
                --       ( BinarySubExpression $
                --           BinaryOperation
                --             (SubTag (DescriptorTerm "descriptor_15"))
                --             Intersect
                --             (SubTag (DescriptorTerm "descriptor_16"))
                --       )
                -- )
                assertEqual
                  ""
                  [file 8]
                  r
            ]
        ]

queryExpressionBasicFunctionality :: IO TaggedConnection -> TestTree
queryExpressionBasicFunctionality c =
  let qqe = flip queryQueryExpression
   in testGroup
        "Query Engine AST Tests - Basic"
        [ testCase "Pattern Wildcard" $ do
            r <- c >>= qqe (fe WildCard)
            -- (FileTermValue "%")
            a <- c >>= allFiles
            assertEqual
              "FileTermValue \"%\" matches all files"
              (HS.fromList a)
              r
        , testCase "Untagged Files" $ do
            r <-
              c
                >>= qqe
                  (fe WildCard -. (tle . tedp . d $ WildCard))
            -- ( BinaryExpression $
            --     BinaryOperation
            --       (FileTermValue "%")
            --       Difference
            --       (TagTermValue (DescriptorTerm "%"))
            -- )
            a <- c >>= queryForUntaggedFiles
            assertEqual
              "Untagged Constant"
              (HS.fromList a)
              r
        , testCase "TagTermValue Expression - Descriptor" $ do
            r <- c >>= qqe (tle . tedp . d . des $ 5)
            -- runExpr (TagTermValue (DescriptorTerm "descriptor_5"))
            a <- c >>= flatQueryForFileByTagDescriptorPattern "descriptor_5"
            assertEqual
              "A TagTermValue performs a flat query"
              (HS.fromList a)
              r
        , testCase "TagTermValue Expression - Descriptor - Flat Query" $ do
            r <- c >>= qqe (tle . tedp . d . des $ 6)
            -- runExpr (TagTermValue (DescriptorTerm "descriptor_6"))
            a <- c >>= flatQueryForFileByTagDescriptorPattern "descriptor_6"
            assertEqual
              "A TagTermValue performs a flat query"
              (HS.fromList a)
              r
        , testCase "TagTermValue Expression - MetaDescriptor" $ do
            r <- c >>= qqe (tle . tedp . rt . des $ 12)
            -- runExpr (TagTermValue (MetaDescriptorTerm "descriptor_12"))
            a <- c >>= flatQueryForFileOnMetaRelationPattern "descriptor_12"
            assertEqual
              "A TagTermValue performs a flat query"
              (HS.fromList a)
              r
        , testCase "TagTermValue Expression - MetaDescriptor - Manual" $ do
            r <- c >>= qqe (tle . tedp . rt . des $ 12)
            -- runExpr (TagTermValue (MetaDescriptorTerm "descriptor_12"))
            assertEqual
              "Should match the case: \"TagTermValue Expression - MetaDescriptor\""
              [file 8, file 9, file 10]
              r
        , testGroup
            "Binary Expressions"
            [ testCase "Union" $ do
                r <-
                  c
                    >>= qqe ((fe . fil $ 1) +. (fe . fil $ 2))
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       (FileTermValue "file_1")
                --       Union
                --       (FileTermValue "file_2")
                -- )
                assertEqual
                  "Union wa union dayo"
                  [file 1, file 2]
                  r
            , testCase "Intersect - Simple Operands" $ do
                r <-
                  c
                    >>= qqe
                      ( (tle . tedp . d . des $ 5)
                          *. (tle . tedp . d . des $ 6)
                      )
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       (TagTermValue (DescriptorTerm "descriptor_5"))
                --       Intersect
                --       (TagTermValue (DescriptorTerm "descriptor_6"))
                -- )
                assertEqual
                  ""
                  [file 4, file 5]
                  r
            , testCase "Intersect - Complex Operand" $ do
                r <-
                  c
                    >>= qqe
                      ( (tle . tedp . d . des $ 5)
                          +. (fe . fil $ 3)
                          *. (tle . tedp . d . des $ 6)
                      )
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       ( BinaryExpression $
                --           BinaryOperation
                --             (TagTermValue (DescriptorTerm "descriptor_5"))
                --             Union
                --             (FileTermValue "file_3")
                --       )
                --       Intersect
                --       (TagTermValue (DescriptorTerm "descriptor_6"))
                -- )
                assertEqual
                  "Binary Operations should be nestable."
                  [file 3, file 4, file 5]
                  r
            , testCase "Intersect - Complex Operand - left-associative" $ do
                r <-
                  c
                    >>= qqe
                      ( ( (tle . tedp . d . des $ 5)
                            +. (fe . fil $ 3)
                        )
                          *. (tle . tedp . d . des $ 6)
                      )
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       ( BinaryExpression $
                --           BinaryOperation
                --             (TagTermValue (DescriptorTerm "descriptor_5"))
                --             Union
                --             (FileTermValue "file_3")
                --       )
                --       Intersect
                --       (TagTermValue (DescriptorTerm "descriptor_6"))
                -- )
                assertEqual
                  "Binary Operations should be nestable."
                  [file 3, file 4, file 5]
                  r
            , testCase "Difference" $ do
                r <-
                  c
                    >>= qqe
                      ( ( (tle . tedp . d . des $ 4)
                            +. ( (tle . tedp . d . des $ 5)
                                  +. (tle . tedp . d . des $ 6)
                               )
                        )
                          -. (tle . tedp . d . des $ 5)
                      )
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       ( BinaryExpression $
                --           BinaryOperation
                --             (TagTermValue (DescriptorTerm "descriptor_4"))
                --             Union
                --             ( BinaryExpression $
                --                 BinaryOperation
                --                   (TagTermValue (DescriptorTerm "descriptor_5"))
                --                   Union
                --                   (TagTermValue (DescriptorTerm "descriptor_6"))
                --             )
                --       )
                --       Difference
                --       (TagTermValue (DescriptorTerm "descriptor_5"))
                -- )
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
                    >>= qqe
                      ( tle
                          ( (tedp . d . des $ 5)
                              # (tedp . d . des $ 6)
                          )
                      )
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_5")
                --       (SubTag (DescriptorTerm "descriptor_6"))
                -- )
                assertEqual
                  "Simple subtag 4{5} should find files with 4{5} tags."
                  [file 4, file 5]
                  r
            , testCase "Flat SubTag TagExpression" $ do
                r <-
                  c
                    >>= qqe
                      ( tle
                          ( (tedp . d . des $ 6)
                              # (tedp . d . des $ 7)
                          )
                      )
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_6")
                --       (SubTag (DescriptorTerm "descriptor_7"))
                -- )
                assertEqual
                  "SubTag queries are a flat operation."
                  [file 5]
                  r
            , testCase "Complex Nested SubTag" $ do
                r <-
                  c
                    >>= qqe
                      ( tle
                          ( (tedp . d . des $ 17)
                              # ( (tedp . d . des $ 18)
                                    # (tedp . d . des $ 20)
                                )
                          )
                      )
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_17")
                --       ( SubExpression $
                --           TagTermExtension
                --             (DescriptorTerm "descriptor_18")
                --             (SubTag (DescriptorTerm "descriptor_20"))
                --       )
                -- )
                assertEqual
                  "SubExpressions modify the supertag environment for lower depths."
                  [file 15]
                  r
            , testCase "SubTag Expression - 1" $ do
                r <-
                  c
                    >>= qqe
                      ( tle
                          ( (tedp . d . des $ 17)
                              # (tedp . d . des $ 18)
                          )
                      )
                --  runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "descriptor_17")
                --       (SubTag (DescriptorTerm "descriptor_18"))
                -- )
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
                        >>= qqe
                          ( tle
                              ( (tedp . d . des $ 17)
                                  # ( (tedp . d . des $ 18)
                                        +. (tedp . d . des $ 19)
                                    )
                              )
                          )
                    -- runExpr
                    -- ( TagExpression $
                    --     TagTermExtension
                    --       (DescriptorTerm "descriptor_17")
                    --       ( BinarySubExpression $
                    --           BinaryOperation
                    --             (SubTag (DescriptorTerm "descriptor_18"))
                    --             Union
                    --             (SubTag (DescriptorTerm "descriptor_19"))
                    --       )
                    -- )
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
                        >>= qqe
                          ( tle
                              ( (tedp . d . des $ 17)
                                  # ( (tedp . d . des $ 18)
                                        *. (tedp . d . des $ 19)
                                    )
                              )
                          )
                    -- runExpr
                    -- ( TagExpression $
                    --     TagTermExtension
                    --       (DescriptorTerm "descriptor_17")
                    --       ( BinarySubExpression $
                    --           BinaryOperation
                    --             (SubTag (DescriptorTerm "descriptor_18"))
                    --             Intersect
                    --             (SubTag (DescriptorTerm "descriptor_19"))
                    --       )
                    -- )
                    assertEqual
                      "SubUnion filters supertags if the supertag\
                      \ is a member of both subtag sets."
                      [ file 13
                      ]
                      r
                , testCase "Sub Difference" $ do
                    r <-
                      c
                        >>= qqe
                          ( tle
                              ( (tedp . d . des $ 17)
                                  # ( (tedp . d . des $ 18)
                                        -. (tedp . d . des $ 19)
                                    )
                              )
                          )
                    -- runExpr
                    -- ( TagExpression $
                    --     TagTermExtension
                    --       (DescriptorTerm "descriptor_17")
                    --       ( BinarySubExpression $
                    --           BinaryOperation
                    --             (SubTag (DescriptorTerm "descriptor_18"))
                    --             Difference
                    --             (SubTag (DescriptorTerm "descriptor_19"))
                    --       )
                    -- )
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
                    >>= qqe
                      ( tle
                          ( (tedp . d $ WildCard)
                              # ( (tedp . d . des $ 20)
                                    -. (tedp . d . des $ 18)
                                )
                          )
                      )
                -- runExpr
                -- ( TagExpression $
                --     TagTermExtension
                --       (DescriptorTerm "%")
                --       ( BinarySubExpression $
                --           BinaryOperation
                --             (SubTag (DescriptorTerm "descriptor_20"))
                --             Difference
                --             (SubTag (DescriptorTerm "descriptor_18"))
                --       )
                -- )
                assertEqual
                  ""
                  [ file 14
                  , file 15
                  -- , file 16 This file is removed by difference
                  ]
                  r
            , testCase "Descriptor Wildcard and SubExpressions - 1" $ do
                r <-
                  c
                    >>= qqe
                      ( tle ((tedp . d $ WildCard) # (tedp . d . des $ 20))
                          -. tle ((tedp . d $ WildCard) # (tedp . d . des $ 18))
                      )
                assertEqual
                  ""
                  [file 14]
                  r
            ]
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
              ( BinaryExpression $
                  BinaryOperation
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
                  ( BinaryExpression $
                      BinaryOperation
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
                  ( BinaryExpression $
                      BinaryOperation
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
                  ( BinaryExpression $
                      BinaryOperation
                        ( BinaryExpression $
                            BinaryOperation
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
                  ( BinaryExpression $
                      BinaryOperation
                        ( BinaryExpression $
                            BinaryOperation
                              (TagTermValue (DescriptorTerm "descriptor_4"))
                              Union
                              ( BinaryExpression $
                                  BinaryOperation
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
                  ( TagExpression $
                      TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
                        (DescriptorTerm "descriptor_17")
                        ( SubExpression $
                            TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
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
                      ( TagExpression $
                          TagTermExtension
                            (DescriptorTerm "descriptor_17")
                            ( BinarySubExpression $
                                BinaryOperation
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
                      ( TagExpression $
                          TagTermExtension
                            (DescriptorTerm "descriptor_17")
                            ( BinarySubExpression $
                                BinaryOperation
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
                      ( TagExpression $
                          TagTermExtension
                            (DescriptorTerm "descriptor_17")
                            ( BinarySubExpression $
                                BinaryOperation
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
                  ( TagExpression $
                      TagTermExtension
                        (DescriptorTerm "%")
                        ( BinarySubExpression $
                            BinaryOperation
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
                  ( TagExpression $
                      TagTermExtension
                        (DescriptorTerm "descriptor_8")
                        ( SubExpression $
                            TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
                        (MetaDescriptorTerm "descriptor_12")
                        ( BinarySubExpression $
                            BinaryOperation
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
                  ( BinaryExpression $
                      BinaryOperation
                        ( TagExpression $
                            TagTermExtension
                              (MetaDescriptorTerm "descriptor_12")
                              (SubTag (DescriptorTerm "descriptor_15"))
                        )
                        Intersect
                        ( TagExpression $
                            TagTermExtension
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
                  ( TagExpression $
                      TagTermExtension
                        (DescriptorTerm "descriptor_13")
                        ( BinarySubExpression $
                            BinaryOperation
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
              SubExpression $
                TagTermExtension
                  (td 21)
                  ( BinarySubExpression $
                      BinaryOperation
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
                SubExpression $
                  TagTermExtension
                    (td 21)
                    ( BinarySubExpression $
                        BinaryOperation
                          ( SubExpression $
                              TagTermExtension
                                (td 22)
                                ( BinarySubExpression $
                                    BinaryOperation
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
                SubExpression $
                  TagTermExtension
                    (td 26)
                    ( BinarySubExpression $
                        BinaryOperation
                          ( SubExpression $
                              TagTermExtension
                                (td 27)
                                ( BinarySubExpression $
                                    BinaryOperation
                                      ( SubExpression $
                                          TagTermExtension
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
              BinarySubExpression $
                BinaryOperation
                  ( BinarySubExpression $
                      BinaryOperation
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
              SubExpression $ TagTermExtension (td 33) (SubTag (td 32))
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

enginePropertyTests :: TestTree
enginePropertyTests =
  testGroup
    "enginePropertyTests"
    [ testGroup
        "Indexing tests"
        [ testProperty
            "Original Expression can be retrieved from flatten"
            ( do
                expr <- arbitrary :: Gen Expression
                return . (==) expr . snd . head . flatten $ expr
            )
        , testProperty
            "Replace an index with itself as an identity"
            ( do
                expr <- arbitrary :: Gen Expression
                n <-
                  suchThat
                    arbitrary
                    (\n' -> isJust . Text.TaggerQL.Expression.Engine.lookup n' $ expr)
                let exprAt' = fromJust $ Text.TaggerQL.Expression.Engine.lookup n expr
                    replaceResult = replace n exprAt' expr
                let tr = expr == replaceResult
                return $
                  whenFail
                    ( do
                        print expr
                        print replaceResult
                    )
                    tr
            )
        , testProperty
            "index works"
            ( do
                expr <- arbitrary :: Gen Expression
                n <-
                  suchThat
                    arbitrary
                    ( \n ->
                        isJust
                          . Text.TaggerQL.Expression.Engine.lookup n
                          $ expr
                    )
                let indexResult = fromJust $ Text.TaggerQL.Expression.Engine.lookup n expr
                    ix' = index indexResult expr
                return $
                  whenFail
                    ( print n
                        >> print indexResult
                        >> print (fmap fst ix')
                        >> print (fmap snd ix')
                    )
                    $ maybe False (\(ixN, ixE) -> ixE == indexResult && ixN == n) ix'
            )
        ]
    ]

instance Arbitrary Expression where
  arbitrary :: Gen Expression
  arbitrary = exprGen

instance Arbitrary SubExpression where
  arbitrary :: Gen SubExpression
  arbitrary = subExprGen

patternGen :: Gen Text
patternGen = T.pack <$> suchThat arbitrary (not . null)

ttGen :: Gen TagTerm
ttGen = oneof (fmap <$> [DescriptorTerm, MetaDescriptorTerm] <*> [patternGen])

subExprGen :: Gen SubExpression
subExprGen = do
  subtags <- SubTag <$> ttGen
  subExpressions <- fmap SubExpression (TagTermExtension <$> ttGen <*> subExprGen)
  binarySubExpressions <-
    fmap
      BinarySubExpression
      ( BinaryOperation <$> subExprGen
          <*> oneof (map pure [Union, Intersect, Difference])
          <*> subExprGen
      )
  oneof . map pure $ [subtags, subExpressions, binarySubExpressions]

exprGen :: Gen Expression
exprGen = do
  tagTermCon <- TagTermValue <$> ttGen
  fileTerm <- FileTermValue . FileTerm <$> patternGen
  tagExpr <- fmap TagExpression (TagTermExtension <$> ttGen <*> subExprGen)
  binaryExpression <-
    fmap
      BinaryExpression
      ( BinaryOperation <$> exprGen
          <*> oneof (map pure [Union, Intersect, Difference])
          <*> exprGen
      )
  oneof . map pure $ [tagTermCon, fileTerm, tagExpr, binaryExpression]