{-# HLINT ignore "Use const" #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Text.TaggerQL.Expression.Engine (
  queryEngineASTTests,
) where

import Data.Either (isRight)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Tagger
import Test.Resources
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Engine
import Text.TaggerQL.Expression.Parser (parseTagExpression)

queryEngineASTTests :: IO TaggedConnection -> TestTree
queryEngineASTTests c =
  testGroup
    "Query Engine AST Tests"
    [ queryExpressionBasicFunctionality c
    , queryExpressionEdgeCases c
    , taggingEngineTests c
    , enginePropertyTests
    , tagDeleteEngineTests c
    , descriptorTreeCreateEngineTests c
    ]

des :: Int -> Pattern
des n = PatternText $ "descriptor_" <> (T.pack . show $ n)

fil :: Int -> Pattern
fil n = PatternText $ "file_" <> (T.pack . show $ n)

queryExpressionEdgeCases :: IO TaggedConnection -> TestTree
queryExpressionEdgeCases c =
  let qqe = flip yuiQLFileQueryExpression
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
                              ∙ ( (tedp . d . des $ 9)
                                    ∙ (tedp . d . des $ 10)
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
                      (tle ((tedp . d . des $ 8) ∙ (tedp . d . des $ 9)))
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
                      (tle ((tedp . d . des $ 9) ∙ (tedp . d . des $ 10)))
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
                      (tle ((tedp . d . des $ 11) ∙ (tedp . d . des $ 9)))
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
                              ∙ ( (tedp . d . des $ 15)
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
                --             Multiplication
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
                      ( tle ((tedp . rt . des $ 12) ∙ (tedp . d . des $ 15))
                          *. tle ((tedp . rt . des $ 12) ∙ (tedp . d . des $ 16))
                      )
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       ( TagExpression $
                --           TagTermExtension
                --             (MetaDescriptorTerm "descriptor_12")
                --             (SubTag (DescriptorTerm "descriptor_15"))
                --       )
                --       Multiplication
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
                              ∙ ( (tedp . d . des $ 15)
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
                --             Multiplication
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
  let qqe = flip yuiQLFileQueryExpression
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
            --       Subtraction
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
            [ testCase "Addition" $ do
                r <-
                  c
                    >>= qqe ((fe . fil $ 1) +. (fe . fil $ 2))
                -- runExpr
                -- ( BinaryExpression $
                --     BinaryOperation
                --       (FileTermValue "file_1")
                --       Addition
                --       (FileTermValue "file_2")
                -- )
                assertEqual
                  "Addition wa union dayo"
                  [file 1, file 2]
                  r
            , testCase "Multiplication - Simple Operands" $ do
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
                --       Multiplication
                --       (TagTermValue (DescriptorTerm "descriptor_6"))
                -- )
                assertEqual
                  ""
                  [file 4, file 5]
                  r
            , testCase "Multiplication - Complex Operand" $ do
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
                --             Addition
                --             (FileTermValue "file_3")
                --       )
                --       Multiplication
                --       (TagTermValue (DescriptorTerm "descriptor_6"))
                -- )
                assertEqual
                  "Binary Operations should be nestable."
                  [file 3, file 4, file 5]
                  r
            , testCase "Multiplication - Complex Operand - left-associative" $ do
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
                --             Addition
                --             (FileTermValue "file_3")
                --       )
                --       Multiplication
                --       (TagTermValue (DescriptorTerm "descriptor_6"))
                -- )
                assertEqual
                  "Binary Operations should be nestable."
                  [file 3, file 4, file 5]
                  r
            , testCase "Subtraction" $ do
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
                --             Addition
                --             ( BinaryExpression $
                --                 BinaryOperation
                --                   (TagTermValue (DescriptorTerm "descriptor_5"))
                --                   Addition
                --                   (TagTermValue (DescriptorTerm "descriptor_6"))
                --             )
                --       )
                --       Subtraction
                --       (TagTermValue (DescriptorTerm "descriptor_5"))
                -- )
                assertEqual
                  "Subtraction wa difference dayo"
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
                              ∙ (tedp . d . des $ 6)
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
                              ∙ (tedp . d . des $ 7)
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
                              ∙ ( (tedp . d . des $ 18)
                                    ∙ (tedp . d . des $ 20)
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
                              ∙ (tedp . d . des $ 18)
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
            , -- , testCase "SubExpression Tags - 0" $ do
              --     h <- c >>= queryTags (DescriptorTerm "descriptor_17")
              --     r <-
              --       c
              --         >>= runReaderT
              --           ( evalSubExpression
              --               (SubTag (DescriptorTerm "descriptor_18"))
              --               (HS.fromList h)
              --           )
              --     assertEqual
              --       "Matches the tags returned by the LHS of the \
              --       \\"TagExpressions - SubBinary Sub Addition\" test."
              --       [ Tag 28 11 17 Nothing
              --       , Tag 32 13 17 Nothing
              --       , Tag 38 15 17 Nothing
              --       , Tag 41 16 17 Nothing
              --       ]
              --       r
              -- , testCase "SubExpression Tags - 1" $ do
              --     h <- c >>= queryTags (DescriptorTerm "descriptor_17")
              --     r <-
              --       c
              --         >>= runReaderT
              --           ( evalSubExpression
              --               (SubTag (DescriptorTerm "descriptor_19"))
              --               (HS.fromList h)
              --           )
              --     assertEqual
              --       "Matches the tags returned by the RHS of the \
              --       \\"TagExpressions - SubBinary Sub Addition\" test."
              --       [ Tag 30 12 17 Nothing
              --       , Tag 32 13 17 Nothing
              --       ]
              --       r
              testGroup
                "TagExpressions - SubBinary"
                [ testCase "Sub Addition" $ do
                    r <-
                      c
                        >>= qqe
                          ( tle
                              ( (tedp . d . des $ 17)
                                  ∙ ( (tedp . d . des $ 18)
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
                    --             Addition
                    --             (SubTag (DescriptorTerm "descriptor_19"))
                    --       )
                    -- )
                    assertEqual
                      "SubAddition filters supertags if the supertag\
                      \ is subtagged by either one or the other subtag sets."
                      [ file 11
                      , file 12
                      , file 13
                      , file 15
                      , file 16
                      ]
                      r
                , testCase "Sub Multiplicationion" $ do
                    r <-
                      c
                        >>= qqe
                          ( tle
                              ( (tedp . d . des $ 17)
                                  ∙ ( (tedp . d . des $ 18)
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
                    --             Multiplication
                    --             (SubTag (DescriptorTerm "descriptor_19"))
                    --       )
                    -- )
                    assertEqual
                      "SubAddition filters supertags if the supertag\
                      \ is a member of both subtag sets."
                      [ file 13
                      ]
                      r
                , testCase "Sub Subtraction" $ do
                    r <-
                      c
                        >>= qqe
                          ( tle
                              ( (tedp . d . des $ 17)
                                  ∙ ( (tedp . d . des $ 18)
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
                    --             Subtraction
                    --             (SubTag (DescriptorTerm "descriptor_19"))
                    --       )
                    -- )
                    assertEqual
                      "SubAddition filters supertags if the supertag\
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
                              ∙ ( (tedp . d . des $ 20)
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
                --             Subtraction
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
                      ( tle ((tedp . d $ WildCard) ∙ (tedp . d . des $ 20))
                          -. tle ((tedp . d $ WildCard) ∙ (tedp . d . des $ 18))
                      )
                assertEqual
                  ""
                  [file 14]
                  r
            ]
        ]

taggingEngineTests :: IO TaggedConnection -> TestTree
taggingEngineTests c =
  let insert se fk c' = yuiQLTagFileExpression c' fk se
   in testGroup
        "Tagging_Engine_Tests"
        [ testCase "Tagging_Engine_Tests_0" $ do
            let se =
                  (pure . des $ 21)
                    ∙ ( (pure . des $ 22)
                          *. (pure . des $ 23)
                      )
                -- SubExpression $
                --   TagTermExtension
                --     (td 21)
                --     ( BinarySubExpression $
                --         BinaryOperation
                --           (SubTag (td 22))
                --           Multiplication
                --           (SubTag (td 23))
                --     )
                fk = 17
                expectedResults =
                  [ Tag 44 17 21 Nothing
                  , Tag 45 17 22 (Just 44)
                  , Tag 46 17 23 (Just 44)
                  ] ::
                    [Tag]
            c
              >>= insert
                se
                fk
            f <- c >>= queryForFileTagsByFileId fk
            assertEqual
              ""
              ( sort
                  expectedResults
              )
              (sort f)
        , after AllSucceed "Tagging_Engine_Tests_0" $
            testCase "Tagging_Engine_Tests_1" $ do
              let se =
                    (pure . des $ 21)
                      ∙ ( ( (pure . des $ 22)
                              ∙ ( (pure . des $ 24)
                                    *. (pure . des $ 25)
                                )
                          )
                            *. (pure . des $ 23)
                        )
                  -- SubExpression $
                  --   TagTermExtension
                  --     (td 21)
                  --     ( BinarySubExpression $
                  --         BinaryOperation
                  --           ( SubExpression $
                  --               TagTermExtension
                  --                 (td 22)
                  --                 ( BinarySubExpression $
                  --                     BinaryOperation
                  --                       (SubTag (td 24))
                  --                       Multiplication
                  --                       (SubTag (td 25))
                  --                 )
                  --           )
                  --           Multiplication
                  --           (SubTag (td 23))
                  --     )
                  fk = 17
                  expectedResults =
                    [ Tag 44 17 21 Nothing
                    , Tag 45 17 22 (Just 44)
                    , Tag 46 17 23 (Just 44)
                    , Tag 47 17 24 (Just 45)
                    , Tag 48 17 25 (Just 45)
                    ]
              c
                >>= insert
                  se
                  fk
              f <- c >>= queryForFileTagsByFileId fk
              assertEqual
                ""
                ( sort
                    expectedResults
                )
                (sort f)
        , after AllSucceed "Tagging_Engine_Tests_1" $
            testCase "Tagging_Engine_Tests_2" $ do
              let se =
                    (pure . des $ 26)
                      ∙ ( ( (pure . des $ 27)
                              ∙ ( ( (pure . des $ 28)
                                      ∙ (pure . des $ 29)
                                  )
                                    *. (pure . des $ 30)
                                )
                          )
                            *. (pure . des $ 31)
                        )
                  -- SubExpression $
                  --   TagTermExtension
                  --     (td 26)
                  --     ( BinarySubExpression $
                  --         BinaryOperation
                  --           ( SubExpression $
                  --               TagTermExtension
                  --                 (td 27)
                  --                 ( BinarySubExpression $
                  --                     BinaryOperation
                  --                       ( SubExpression $
                  --                           TagTermExtension
                  --                             (td 28)
                  --                             (SubTag (td 29))
                  --                       )
                  --                       Multiplication
                  --                       (SubTag (td 30))
                  --                 )
                  --           )
                  --           Multiplication
                  --           (SubTag (td 31))
                  --     )
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
                >>= insert
                  se
                  fk
              f <- c >>= queryForFileTagsByFileId fk
              assertEqual
                ""
                ( sort
                    expectedResults
                )
                (sort f)
        , after AllSucceed "Tagging_Engine_Tests_2"
            . testCase "Tagging_Engine_Tests_3"
            $ do
              let se =
                    ( (pure . des $ 32)
                        *. (pure . des $ 33)
                    )
                      *. (pure . des $ 34)
                  -- BinarySubExpression $
                  --   BinaryOperation
                  --     ( BinarySubExpression $
                  --         BinaryOperation
                  --           (SubTag (td 32))
                  --           Multiplication
                  --           (SubTag (td 33))
                  --     )
                  --     Multiplication
                  --     (SubTag (td 34))
                  fk = 19
                  expectedResults =
                    [ Tag 55 19 32 Nothing
                    , Tag 56 19 33 Nothing
                    , Tag 57 19 34 Nothing
                    ]
              c
                >>= insert
                  se
                  fk
              f <- c >>= queryForFileTagsByFileId fk
              assertEqual
                ""
                ( sort
                    expectedResults
                )
                (sort f)
        , after AllSucceed "Tagging_Engine_Tests_3"
            . testCase "Tagging_Engine_Tests_4"
            $ do
              let se =
                    (pure . des $ 33)
                      ∙ (pure . des $ 32)
                  -- SubExpression $ TagTermExtension (td 33) (SubTag (td 32))
                  fk = 19
                  expectedResults =
                    [ Tag 55 19 32 Nothing
                    , Tag 56 19 33 Nothing
                    , Tag 57 19 34 Nothing
                    , Tag 58 19 32 (Just 56)
                    ]
              c
                >>= insert
                  se
                  fk
              f <- c >>= queryForFileTagsByFileId fk
              assertEqual
                ""
                ( sort
                    expectedResults
                )
                (sort f)
        , after AllSucceed "Tagging_Engine_Tests_4"
            . testCase "Tagging_Engine_Tests_5"
            $ do
              let se =
                    ( (pure . des $ 35)
                        *. (pure . des $ 36)
                    )
                      ∙ (pure . des $ 37)
                  fk = 20
                  expectedResults =
                    [ Tag 59 20 35 Nothing
                    , Tag 60 20 37 (Just 59)
                    , Tag 61 20 36 Nothing
                    , Tag 62 20 37 (Just 61)
                    ]
              c >>= insert se fk
              f <- c >>= queryForFileTagsByFileId fk
              assertEqual
                "Tagging should work with a lift-distributive expression."
                (sort expectedResults)
                (sort f)
        , after AllSucceed "Tagging_Engine_Tests_5"
            . testCase "Tagging_Engine_Tests_6"
            $ do
              let se =
                    ( (pure . des $ 38)
                        *. ( (pure . des $ 39)
                              ∙ (pure . des $ 40)
                           )
                    )
                      ∙ (pure . des $ 41)
                  fk = 21
                  expectedResults =
                    [ Tag 63 21 38 Nothing
                    , Tag 64 21 41 (Just 63)
                    , Tag 65 21 39 Nothing
                    , Tag 66 21 40 (Just 65)
                    , Tag 67 21 41 (Just 66)
                    ]
              c >>= insert se fk
              f <- c >>= queryForFileTagsByFileId fk
              assertEqual
                "Tagging should work with a lift-distributive expression."
                (sort expectedResults)
                (sort f)
        ]

tagDeleteEngineTests :: HasCallStack => IO TaggedConnection -> TestTree
tagDeleteEngineTests ioc =
  after AllSucceed "Tagging_Engine_Tests" $
    testGroup
      "Tagging_Delete_Tests"
      [ testGroup
          "Tagging_Delete_Tests_Setup"
          []
      , after AllSucceed "Tagging_Delete_Tests_Setup" $
          testGroup
            "Tagging_Delete_Tests_Tests"
            [ tagDeleteTestCase
                "Single Term Delete"
                [17]
                "%25"
                [48]
            , tagDeleteTestCase
                "Specific Term Delete"
                [17]
                "%21{%22{%25}}"
                [48]
            , tagDeleteTestCase
                "Delete Two Terms"
                [17]
                "%21{%23} %21{%22{%25}}"
                [46, 48]
            , tagDeleteTestCase
                "Distribute Tag Deletion"
                [17]
                "%21{%23 %22{%25}}"
                [46, 48]
            , tagDeleteTestCase
                "Higher Level Tag"
                [17]
                "%21"
                [44]
            , tagDeleteTestCase
                "Multiple Files With Disjoint Tags"
                [17, 18]
                "%21 %26"
                [44, 49]
            , tagDeleteTestCase
                "Multiple Files With Similar Tags"
                [11, 12, 13, 14, 15, 16]
                "%17{%18}"
                [29, 33, 39, 42]
            , tagDeleteTestCase
                "Multiple Files With Similar Tags - 1"
                [11, 12, 13, 14, 15, 16, 17]
                "%18"
                [29, 33, 35, 39, 42]
            ]
      ]
 where
  tagDeleteTestCase ::
    HasCallStack =>
    String ->
    [RecordKey File] ->
    Text ->
    HashSet (RecordKey Tag) ->
    TestTree
  tagDeleteTestCase name fks tqe expected =
    testCase name $ do
      tagsToDelete <- fmap (fmap (HS.map tagId)) . sequenceA $ do
        deleteExpression <- fmap runDTerm <$> parseTagExpression tqe
        pure $
          ioc >>= \c ->
            yuiQLQueryTagDeleteExpression c fks deleteExpression

      assertEqual "" (Right expected) tagsToDelete

descriptorTreeCreateEngineTests :: IO TaggedConnection -> TestTree
descriptorTreeCreateEngineTests ioc =
  after AllSucceed "Tagging_Delete_Tests" $
    testGroup
      "Descriptor_Tree_Create_Tests"
      [ testCaseSteps "Create_One_Descriptor" $ \step -> do
          c <- ioc

          step "Insert Descriptors"
          r <- yuiQLCreateDescriptors c "script_1"
          assertBool "Parse failure" (isRight r)

          step "Query Descriptor"
          ds <- queryForDescriptorByPattern "script_1" c
          assertEqual "Could not find \"script_1\"" ["script_1"] (descriptor <$> ds)
      , after AllSucceed "Create_One_Descriptor" $
          testCaseSteps "Create_Simple_Descriptor_Tree" $ \step -> do
            c <- ioc

            step "Insert Descriptors"
            r <- yuiQLCreateDescriptors c "script_2 {script_3}"
            assertBool "Parse failure" (isRight r)

            step "Query Head Descriptor"
            s2 <- queryForDescriptorByPattern "script_2" c
            assertEqual "Insert \"script_2\"" ["script_2"] (descriptor <$> s2)

            step "Query Head Infra Descriptors"
            infraS3 <- getInfraChildren (descriptorId . head $ s2) c
            assertEqual
              "\"script_3\" is infra to \"script_2\""
              ["script_3"]
              (descriptor <$> infraS3)
      , after AllSucceed "Create_Simple_Descriptor_Tree" $
          testCaseSteps "Update_Descriptor_Tree_With_Existing_Descriptor" $ \step -> do
            c <- ioc

            step "Insert Descriptors"
            r <- yuiQLCreateDescriptors c "script_1 {script_3}"
            assertBool "Parse failure" (isRight r)

            step "Query Head Descriptor"
            s1 <- queryForDescriptorByPattern "script_1" c
            assertEqual "find \"script_1\"" ["script_1"] (descriptor <$> s1)

            step "Query Head Infra Descriptors"
            infraS3 <- getInfraChildren (descriptorId . head $ s1) c
            assertEqual
              "\"script_3\" is infra to \"script_2\""
              ["script_3"]
              (descriptor <$> infraS3)

            step "Query Previous Head Infra Descriptors"
            s2 <- queryForDescriptorByPattern "script_2" c
            assertEqual "Could not find \"script_2\"" ["script_2"] (descriptor <$> s2)
            s2Children <- getInfraChildren (descriptorId . head $ s2) c
            assertEqual "\"script_2\" has infra children when it shouldn't" [] s2Children
      ]

file :: RecordKey File -> File
file n = File n ("file_" <> (T.pack . show $ n))

enginePropertyTests :: TestTree
enginePropertyTests =
  testGroup
    "enginePropertyTests"
    []
