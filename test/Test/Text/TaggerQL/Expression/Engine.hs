{-# HLINT ignore "Use const" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.Text.TaggerQL.Expression.Engine (
  queryEngineASTTests,
) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust, isJust)
import Data.String (IsString, fromString)
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
    [ fExpressionBasicQueries c
    , fExpressionEdgeCases c
    , tExpressionTaggingTests c
    , enginePropertyTests
    ]

runFExpr ::
  FExpression (DTerm Pattern) Pattern ->
  TaggedConnection ->
  IO (HS.HashSet File)
runFExpr e = runReaderT (interpretQuery e)

runQueryTags :: DTerm Pattern -> TaggedConnection -> IO (HS.HashSet Tag)
runQueryTags d = runReaderT (queryTagSet' d)

l :: TExpression b -> FExpression b a
l = LiftTExpression

dmt :: a -> TExpression (DTerm a)
dmt = TValue . DMetaTerm

dt :: a -> TExpression (DTerm a)
dt = TValue . DTerm

fExpressionEdgeCases :: IO TaggedConnection -> TestTree
fExpressionEdgeCases c =
  testGroup
    "Query Engine AST - Edge Cases"
    [ testGroup
        "Tech-note f02a13240b for files, A: a{b{c}} and B: a{b} d{b{c}}"
        [ testCase "Return A for query a{b{c}}" $ do
            r <-
              c
                >>= runFExpr
                  ( l
                      ( TExpressionDistribution
                          ( DTerm "descriptor_8"
                              :$ TExpressionDistribution
                                ( DTerm "descriptor_9"
                                    :$ (TValue . DTerm $ "descriptor_10")
                                )
                          )
                      )
                      -- TagExpression $
                      --   TagTermExtension
                      --     (DescriptorTerm "descriptor_8")
                      --     ( SubExpression $
                      --         TagTermExtension
                      --           (DescriptorTerm "descriptor_9")
                      --           (SubTag (DescriptorTerm "descriptor_10"))
                      --     )
                  )
            assertEqual
              ""
              [file 6]
              r
        , testCase "Return A, B for a{b}" $ do
            r <-
              c
                >>= runFExpr
                  ( l (TExpressionDistribution (DTerm "descriptor_8" :$ (TValue . DTerm $ "descriptor_9")))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (DescriptorTerm "descriptor_8")
                  --     (SubTag (DescriptorTerm "descriptor_9"))
                  )
            assertEqual
              ""
              [file 6, file 7]
              r
        , testCase "Return A, B for b{c}" $ do
            r <-
              c
                >>= runFExpr
                  ( l (TExpressionDistribution (DTerm "descriptor_9" :$ (TValue . DTerm $ "descriptor_10")))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (DescriptorTerm "descriptor_9")
                  --     (SubTag (DescriptorTerm "descriptor_10"))
                  )
            assertEqual
              ""
              [file 6, file 7]
              r
        , testCase "Return B for d{b}" $ do
            r <-
              c
                >>= runFExpr
                  ( l (TExpressionDistribution (DTerm "descriptor_11" :$ (TValue . DTerm $ "descriptor_9")))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (DescriptorTerm "descriptor_11")
                  --     (SubTag (DescriptorTerm "descriptor_9"))
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
                >>= runFExpr
                  ( l (TExpressionDistribution (DMetaTerm "descriptor_12" :$ ((TValue . DTerm $ "descriptor_15") <^> (TValue . DTerm $ "descriptor_16"))))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (MetaDescriptorTerm "descriptor_12")
                  --     ( BinarySubExpression $
                  --         BinaryOperation
                  --           (SubTag (DescriptorTerm "descriptor_15"))
                  --           Intersect
                  --           (SubTag (DescriptorTerm "descriptor_16"))
                  --     )
                  )
            assertEqual
              ""
              [file 8, file 9]
              r
        , testCase "Disjoint, single arity, relational subqueries are a superset" $ do
            r <-
              c
                >>= runFExpr
                  ( l (dmt "descriptor_12" @> dt "descriptor_15")
                      <^> l (dmt "descriptor_12" @> dt "descriptor_16")
                      -- BinaryExpression $
                      --   BinaryOperation
                      --     ( TagExpression $
                      --         TagTermExtension
                      --           (MetaDescriptorTerm "descriptor_12")
                      --           (SubTag (DescriptorTerm "descriptor_15"))
                      --     )
                      --     Intersect
                      --     ( TagExpression $
                      --         TagTermExtension
                      --           (MetaDescriptorTerm "descriptor_12")
                      --           (SubTag (DescriptorTerm "descriptor_16"))
                      --     )
                  )
            assertEqual
              ""
              [file 8, file 9, file 10]
              r
        , testCase "Descriptor subqueries are not disjoint" $ do
            r <-
              c
                >>= runFExpr
                  ( l (dt "descriptor_13" @> (dt "descriptor_15" <^> dt "descriptor_16"))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (DescriptorTerm "descriptor_13")
                  --     ( BinarySubExpression $
                  --         BinaryOperation
                  --           (SubTag (DescriptorTerm "descriptor_15"))
                  --           Intersect
                  --           (SubTag (DescriptorTerm "descriptor_16"))
                  --     )
                  )
            assertEqual
              ""
              [file 8]
              r
        ]
    ]

fExpressionBasicQueries :: IO TaggedConnection -> TestTree
fExpressionBasicQueries c =
  testGroup
    "Query Engine AST Tests - Basic"
    [ testCase "Pattern Wildcard" $ do
        r <- c >>= runFExpr (FValue WildCard)
        a <- c >>= allFiles
        assertEqual
          "FileTermValue \"%\" matches all files"
          (HS.fromList a)
          r
    , testCase "mid is all files" $ do
        r <- c >>= runFExpr mid
        a <- c >>= allFiles
        assertEqual
          "The multiplicative identity for a query FExpression is all files"
          (HS.fromList a)
          r
    , testCase "Untagged Files" $ do
        r <-
          c
            >>= runFExpr
              (FValue WildCard <-> (l . TValue . DTerm $ WildCard))
        a <- c >>= queryForUntaggedFiles
        assertEqual
          "Untagged Constant"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - Descriptor" $ do
        r <- c >>= runFExpr (l . TValue $ DTerm "descriptor_5")
        a <- c >>= flatQueryForFileByTagDescriptorPattern "descriptor_5"
        assertEqual
          "A TagTermValue performs a flat query"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - Descriptor - Flat Query" $ do
        r <- c >>= runFExpr (l . TValue $ (DTerm "descriptor_6"))
        a <- c >>= flatQueryForFileByTagDescriptorPattern "descriptor_6"
        assertEqual
          "A TagTermValue performs a flat query"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - MetaDescriptor" $ do
        r <- c >>= runFExpr (l . TValue $ (DMetaTerm "descriptor_12"))
        a <- c >>= flatQueryForFileOnMetaRelationPattern "descriptor_12"
        assertEqual
          "A TagTermValue performs a flat query"
          (HS.fromList a)
          r
    , testCase "TagTermValue Expression - MetaDescriptor - Manual" $ do
        r <- c >>= runFExpr (l . TValue $ (DMetaTerm "descriptor_12"))
        assertEqual
          "Should match the case: \"TagTermValue Expression - MetaDescriptor\""
          [file 8, file 9, file 10]
          r
    , testGroup
        "Binary Expressions"
        [ testCase "Union" $ do
            r <-
              c
                >>= runFExpr
                  ( FValue "file_1" <+> FValue "file_2"
                  )
            assertEqual
              "Union wa union dayo"
              [file 1, file 2]
              r
        , testCase "Intersect - Simple Operands" $ do
            r <-
              c
                >>= runFExpr
                  ( (l . TValue . DTerm $ "descriptor_5") <^> (l . TValue . DTerm $ "descriptor_6")
                  )
            assertEqual
              ""
              [file 4, file 5]
              r
        , testCase "Intersect - Complex Operand" $ do
            r <-
              c
                >>= runFExpr
                  ( (l . TValue . DTerm $ "descriptor_5") <+> FValue "file_3" <^> (l . TValue . DTerm $ "descriptor_6")
                  )
            assertEqual
              "Binary Operations should be nestable."
              [file 3, file 4, file 5]
              r
        , testCase "Difference" $ do
            r <-
              c
                >>= runFExpr
                  ( (l . TValue . DTerm $ "descriptor_4")
                      <+> ((l . TValue . DTerm $ "descriptor_5") <+> (l . TValue . DTerm $ "descriptor_6"))
                      <-> (l . TValue . DTerm $ "descriptor_5")
                      -- BinaryExpression $
                      --   BinaryOperation
                      --     ( BinaryExpression $
                      --         BinaryOperation
                      --           (TagTermValue (DescriptorTerm "descriptor_4"))
                      --           Union
                      --           ( BinaryExpression $
                      --               BinaryOperation
                      --                 (TagTermValue (DescriptorTerm "descriptor_5"))
                      --                 Union
                      --                 (TagTermValue (DescriptorTerm "descriptor_6"))
                      --           )
                      --     )
                      --     Difference
                      --     (TagTermValue (DescriptorTerm "descriptor_5"))
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
                >>= runFExpr
                  ( l (TExpressionDistribution (DTerm "descriptor_5" :$ (TValue . DTerm $ "descriptor_6")))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (DescriptorTerm "descriptor_5")
                  --     (SubTag (DescriptorTerm "descriptor_6"))
                  )
            assertEqual
              "Simple subtag 5{6} should find files with 5{6} tags."
              [file 4, file 5]
              r
        , testCase "Flat SubTag TagExpression" $ do
            r <-
              c
                >>= runFExpr
                  ( l (TExpressionDistribution (DTerm "descriptor_6" :$ (TValue . DTerm $ "descriptor_7")))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (DescriptorTerm "descriptor_6")
                  --     (SubTag (DescriptorTerm "descriptor_7"))
                  )
            assertEqual
              "SubTag queries are a flat operation."
              [file 5]
              r
        , testCase "Complex Nested SubTag" $ do
            r <-
              c
                >>= runFExpr
                  ( l
                      ( TExpressionDistribution
                          ( DTerm "descriptor_17"
                              :$ TExpressionDistribution
                                ( DTerm "descriptor_18"
                                    :$ (TValue . DTerm $ "descriptor_20")
                                )
                          )
                      )
                      -- TagExpression $
                      --   TagTermExtension
                      --     (DescriptorTerm "descriptor_17")
                      --     ( SubExpression $
                      --         TagTermExtension
                      --           (DescriptorTerm "descriptor_18")
                      --           (SubTag (DescriptorTerm "descriptor_20"))
                      --     )
                  )
            assertEqual
              "SubExpressions modify the supertag environment for lower depths."
              [file 15]
              r
        , testCase "SubTag Expression - 1" $ do
            r <-
              c
                >>= runFExpr
                  ( l (TExpressionDistribution (DTerm "descriptor_17" :$ (TValue . DTerm $ "descriptor_18")))
                  -- TagExpression $
                  --   TagTermExtension
                  --     (DescriptorTerm "descriptor_17")
                  --     (SubTag (DescriptorTerm "descriptor_18"))
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
            h <- c >>= runQueryTags (DTerm "descriptor_17")
            r <-
              c
                >>= runReaderT
                  ( evalSubExpression
                      (TValue (DTerm "descriptor_18"))
                      h
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
            h <- c >>= runQueryTags (DTerm "descriptor_17")
            r <-
              c
                >>= runReaderT
                  ( evalSubExpression
                      (TValue (DTerm "descriptor_19"))
                      h
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
                    >>= runFExpr
                      ( l (TExpressionDistribution (DTerm "descriptor_17" :$ ((TValue . DTerm $ "descriptor_18") <+> (TValue . DTerm $ "descriptor_19"))))
                      -- TagExpression $
                      --   TagTermExtension
                      --     (DescriptorTerm "descriptor_17")
                      --     ( BinarySubExpression $
                      --         BinaryOperation
                      --           (SubTag (DescriptorTerm "descriptor_18"))
                      --           Union
                      --           (SubTag (DescriptorTerm "descriptor_19"))
                      --     )
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
                    >>= runFExpr
                      ( l (TExpressionDistribution (DTerm "descriptor_17" :$ ((TValue . DTerm $ "descriptor_18") <^> (TValue . DTerm $ "descriptor_19"))))
                      -- TagExpression $
                      --   TagTermExtension
                      --     (DescriptorTerm "descriptor_17")
                      --     ( BinarySubExpression $
                      --         BinaryOperation
                      --           (SubTag (DescriptorTerm "descriptor_18"))
                      --           Intersect
                      --           (SubTag (DescriptorTerm "descriptor_19"))
                      --     )
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
                    >>= runFExpr
                      ( l (TExpressionDistribution (DTerm "descriptor_17" :$ ((TValue . DTerm $ "descriptor_18") <-> (TValue . DTerm $ "descriptor_19"))))
                      -- TagExpression $
                      --   TagTermExtension
                      --     (DescriptorTerm "descriptor_17")
                      --     ( BinarySubExpression $
                      --         BinaryOperation
                      --           (SubTag (DescriptorTerm "descriptor_18"))
                      --           Difference
                      --           (SubTag (DescriptorTerm "descriptor_19"))
                      --     )
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
                >>= runFExpr
                  ( l
                      ( TExpressionDistribution
                          ( DTerm WildCard
                              :$ ( (TValue . DTerm $ "descriptor_20")
                                    <-> (TValue . DTerm $ "descriptor_18")
                                 )
                          )
                      )
                      -- TagExpression $
                      --   TagTermExtension
                      --     (DescriptorTerm "%")
                      --     ( BinarySubExpression $
                      --         BinaryOperation
                      --           (SubTag (DescriptorTerm "descriptor_20"))
                      --           Difference
                      --           (SubTag (DescriptorTerm "descriptor_18"))
                      --     )
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

tExpressionTaggingTests :: IO TaggedConnection -> TestTree
tExpressionTaggingTests c =
  testGroup
    "Tagging Engine Tests"
    [ testCase "Tagging Engine - 0" $ do
        let se =
              -- 21 {22 23}
              TExpressionDistribution (td 21 :$ ((TValue . td $ 22) <^> (TValue . td $ 23)))
            -- SubExpression $
            --   TagTermExtension
            --     (td 21)
            --     ( BinarySubExpression $
            --         BinaryOperation
            --           (SubTag (td 22))
            --           Intersect
            --           (SubTag (td 23))
            --     )
            fk = 17
            expectedResults =
              [ Tag 44 17 21 Nothing
              , Tag 45 17 22 (Just 44)
              , Tag 46 17 23 (Just 44)
              ]
        c
          >>= applyTExpressionToFile
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
                -- 21 {22 {24 25} 23}
                TExpressionDistribution
                  ( td 21
                      :$ ( TExpressionDistribution
                            ( td 22
                                :$ ((TValue . td $ 24) <^> (TValue . td $ 25))
                            )
                            <^> (TValue . td $ 23)
                         )
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
              --                       Intersect
              --                       (SubTag (td 25))
              --                 )
              --           )
              --           Intersect
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
            >>= applyTExpressionToFile
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
                -- 26 {27 {28 {29} 30} 31}
                TExpressionDistribution
                  ( td 26
                      :$ ( TExpressionDistribution
                            ( td 27
                                :$ ( TExpressionDistribution
                                      ( td 28
                                          :$ (TValue . td $ 29)
                                      )
                                      <^> (TValue . td $ 30)
                                   )
                            )
                            <^> (TValue . td $ 31)
                         )
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
              --                       Intersect
              --                       (SubTag (td 30))
              --                 )
              --           )
              --           Intersect
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
            >>= applyTExpressionToFile
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
              -- 32 33 34
              (TValue . td $ 32) <^> (TValue . td $ 33) <^> (TValue . td $ 34)
            -- BinarySubExpression $
            --   BinaryOperation
            --     ( BinarySubExpression $
            --         BinaryOperation
            --           (SubTag (td 32))
            --           Intersect
            --           (SubTag (td 33))
            --     )
            --     Intersect
            --     (SubTag (td 34))
            fk = 19
            expectedResults =
              [ Tag 55 19 32 Nothing
              , Tag 56 19 33 Nothing
              , Tag 57 19 34 Nothing
              ]
        c
          >>= applyTExpressionToFile
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
              -- 33 {32}
              TExpressionDistribution (td 33 :$ (TValue . td $ 32))
            -- SubExpression $ TagTermExtension (td 33) (SubTag (td 32))
            fk = 19
            expectedResults =
              [ Tag 55 19 32 Nothing
              , Tag 56 19 33 Nothing
              , Tag 57 19 34 Nothing
              , Tag 58 19 32 (Just 56)
              ]
        c
          >>= applyTExpressionToFile
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

td :: IsString s => Int -> s
td n = fromString $ "descriptor_" <> show n

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