{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test.Text.TaggerQL.Expression.AST (
    astTests,
) where

import Control.Monad ((>=>))
import Test.Resources
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.TaggerQL.Expression.AST

astTests :: TestTree
astTests =
    testGroup
        "AST Tests"
        [astProperties, astEditorProperties]

astProperties :: TestTree
astProperties =
    testGroup
        "LabeledFreeTree Properties"
        [ testGroup
            "LabeledFreeTree Properties"
            [ testProperty
                "Left Monad Identity"
                ( do
                    f <- arbitrary :: Gen (Int -> LabeledFreeTree Int Int)
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- arbitrary :: Gen (LabeledFreeTree Int Int)
                    let testProp = (re >>= pure) == re
                    pure testProp
                )
            , testProperty
                "Kleisli Arrow Associativity"
                ( do
                    let genF =
                            suchThat
                                -- Use relatively small functions so it doesn't
                                -- take forever to run.
                                (resize 35 arbitrary)
                                -- Exclude functions that appear to be identities
                                (\(Fn fun) -> fun 1 /= pure 1) ::
                                Gen (Fun Int (LabeledFreeTree Int Int))
                    f <- genF
                    g <- genF
                    h <- genF
                    dt <- arbitrary :: Gen Int
                    let testProp =
                            ( applyFun f
                                >=> ( applyFun g
                                        >=> applyFun h
                                    )
                            )
                                dt
                                == ( ( applyFun f
                                        >=> applyFun g
                                     )
                                        >=> applyFun h
                                   )
                                    dt
                    pure testProp
                )
            ]
        , testGroup
            "FreeDisjunctMonad Properties"
            [ testProperty
                "Left Monad Identity"
                ( do
                    f <- (resize 3 arbitrary :: Gen (Int -> FreeDisjunctMonad (LabeledFreeTree Int) (LabeledFreeTree Int) Int))
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- resize 3 arbitrary :: Gen (FreeDisjunctMonad (LabeledFreeTree Int) (LabeledFreeTree Int) Int)
                    let testProp = (re >>= pure) == re
                    pure testProp
                )
            , testProperty
                "Kleisli Arrow Associativity"
                ( do
                    let genF =
                            suchThat
                                -- Use relatively small functions so it doesn't
                                -- take forever to run.
                                (resize 3 arbitrary)
                                -- Exclude functions that appear to be identities
                                (\(Fn fun) -> fun 1 /= pure 1) ::
                                Gen (Fun Int (FreeDisjunctMonad (LabeledFreeTree Int) (LabeledFreeTree Int) Int))
                    f <- genF
                    g <- genF
                    h <- genF
                    dt <- arbitrary :: Gen Int
                    let testProp =
                            ( applyFun f
                                >=> ( applyFun g
                                        >=> applyFun h
                                    )
                            )
                                dt
                                == ( ( applyFun f
                                        >=> applyFun g
                                     )
                                        >=> applyFun h
                                   )
                                    dt
                    pure testProp
                )
            ]
        , testGroup
            "DTerm properties"
            [ testProperty
                "Left Monad Identity"
                ( do
                    f <- arbitrary :: Gen (Int -> DTerm Int)
                    i <- arbitrary :: Gen Int
                    pure $ (pure i >>= f) == f i
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    dt <- arbitrary :: Gen (DTerm Int)
                    pure $ (dt >>= pure) == dt
                )
            , testProperty
                "Kleisli Arrow Associativity"
                ( do
                    let genF =
                            suchThat
                                -- Use relatively small functions so it doesn't
                                -- take forever to run.
                                (resize 35 arbitrary)
                                -- Exclude functions that appear to be identities
                                (\(Fn fun) -> fun 1 /= pure 1) ::
                                Gen (Fun Int (DTerm Int))
                    f <- genF
                    g <- genF
                    h <- genF
                    dt <- resize 10 arbitrary :: Gen Int
                    let testProp =
                            ( applyFun f
                                >=> ( applyFun g
                                        >=> applyFun h
                                    )
                            )
                                dt
                                == ( ( applyFun f
                                        >=> applyFun g
                                     )
                                        >=> applyFun h
                                   )
                                    dt
                    pure testProp
                )
            ]
        , testGroup
            "QueryExpression Properties"
            [ testCase "Unification Distribution is Right-Associative" $
                assertEqual
                    "(a & p.b){c}{d} = a{c{d}} & (p.b & c{d})"
                    ( ( Node . Right $
                            ( (tedp . rt $ "a")
                                ∙ ( (tedp . rt $ "c")
                                        ∙ (tedp . rt $ "d")
                                  )
                            )
                      )
                        *. ( (Node . Left $ "b")
                                *. ( Node . Right $
                                        ( (tedp . rt $ "c")
                                            ∙ (tedp . rt $ "d")
                                        )
                                   )
                           )
                    )
                    ( simplifyQueryExpression $
                        ( liftSimpleQueryRing
                            ( (pure . Right . tedp . rt $ "a")
                                *. (pure . Left $ "b")
                            )
                            <-# (tedp . rt $ "c")
                        )
                            <-# (tedp . rt $ "d")
                    )
            , testCase "Unification Distribution is Associatively Right-Associative" $
                assertEqual
                    "(a & p.b){c{d}} = a{c{d}} & (p.b & {c{d}})"
                    ( ( Node . Right $
                            ( (tedp . rt $ "a")
                                ∙ ( (tedp . rt $ "c")
                                        ∙ (tedp . rt $ "d")
                                  )
                            )
                      )
                        *. ( (Node . Left $ "b")
                                *. ( Node . Right $
                                        ( (tedp . rt $ "c")
                                            ∙ (tedp . rt $ "d")
                                        )
                                   )
                           )
                    )
                    ( simplifyQueryExpression $
                        liftSimpleQueryRing
                            ( (pure . Right . tedp . rt $ "a")
                                *. (pure . Left $ "b")
                            )
                            <-# ( (tedp . rt $ "c")
                                    ∙ (tedp . rt $ "d")
                                )
                    )
            ]
        ]

astEditorProperties :: TestTree
astEditorProperties =
    testCase "This is not implemented" (assertBool "" True)

-- testGroup
--     "AST Editor Properties"
--     [ testGroup
--         "Replacing Indices With Themselves"
--         [ testProperty
--             "QueryExpression"
--             ( do
--                 expr <- resize 3 arbitrary
--                 n <- suchThat arbitrary (\n' -> isJust $ findQueryExpression n' expr)
--                 let exprAt = fromJust $ findQueryExpression n expr
--                     replaceResult = withQueryExpression n expr (const exprAt)
--                 let propTest = expr == replaceResult
--                 pure propTest
--             )
--         , testProperty
--             "TagExpression"
--             ( do
--                 expr <-
--                     resize 3 arbitrary ::
--                         Gen TagQueryExpression
--                 n <- suchThat arbitrary (\n' -> isJust $ findTagExpression n' expr)
--                 let exprAt = fromJust $ findTagExpression n expr
--                     replaceResult = normalize $ withTagExpression n expr (const exprAt)
--                 let propTest = normalize expr == replaceResult
--                 pure $
--                     whenFail
--                         ( do
--                             print n
--                             print exprAt
--                             print expr
--                             print replaceResult
--                         )
--                         propTest
--             )
--         ]
--     ]