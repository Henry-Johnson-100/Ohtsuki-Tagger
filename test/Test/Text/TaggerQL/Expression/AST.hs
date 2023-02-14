{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test.Text.TaggerQL.Expression.AST (
    astTests,
) where

import Control.Monad ((>=>))
import Data.Coerce (coerce)
import Data.Maybe (fromJust, isJust)
import Test.Resources (
    QCDTerm (..),
    QCFreeCompoundExpression (runQCFreeCompoundExpression),
    QCFreeMagma (QCFreeMagma),
    QCPattern (..),
    QCQueryLeaf (..),
    QCRingExpression (..),
 )
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.TaggerQL.Expression.AST (
    DTerm,
    FreeMagma,
    Pattern,
    QueryExpression (QueryExpression),
    QueryLeaf,
    RingExpression,
    TagQueryExpression,
    mapK,
    mapT,
    normalize,
 )
import Text.TaggerQL.Expression.AST.Editor (
    findQueryExpression,
    findTagExpression,
    withQueryExpression,
    withTagExpression,
 )

astTests :: TestTree
astTests =
    testGroup
        "AST Tests"
        [astProperties, astEditorProperties]

astProperties :: TestTree
astProperties =
    testGroup
        "Expression Properties"
        [ testGroup
            "RingExpression Properties"
            [ testProperty
                "Left Monad Identity"
                ( do
                    f <- arbitrary :: Gen (Int -> QCRingExpression Int)
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- arbitrary :: Gen (QCRingExpression Int)
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
                                Gen (Fun Int (QCRingExpression Int))
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
            "FreeMagma Properties"
            [ testProperty
                "Left Monad Identity"
                ( do
                    f <- arbitrary :: Gen (Int -> QCFreeMagma Int)
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- arbitrary :: Gen (QCFreeMagma Int)
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
                                Gen (Fun Int (QCFreeMagma Int))
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
            "TagExpression Properties"
            [ testProperty
                "Left Monad Identity"
                ( do
                    f <- (resize 3 arbitrary :: Gen (Int -> QCFreeCompoundExpression QCRingExpression QCFreeMagma Int))
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- resize 3 arbitrary :: Gen (QCFreeCompoundExpression QCRingExpression QCFreeMagma Int)
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
                                Gen (Fun Int (QCFreeCompoundExpression QCRingExpression QCFreeMagma Int))
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
                    f <- arbitrary :: Gen (Int -> QCDTerm Int)
                    i <- arbitrary :: Gen Int
                    pure $ (pure i >>= f) == f i
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    dt <- arbitrary :: Gen (QCDTerm Int)
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
                                Gen (Fun Int (QCDTerm Int))
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
        ]

astEditorProperties :: TestTree
astEditorProperties =
    testGroup
        "AST Editor Properties"
        [ testGroup
            "Replacing Indices With Themselves"
            [ testProperty
                "QueryExpression"
                ( do
                    expr <-
                        QueryExpression
                            . ( coerce ::
                                    QCRingExpression QCQueryLeaf ->
                                    RingExpression QueryLeaf
                              )
                            <$> resize 3 arbitrary ::
                            Gen QueryExpression
                    n <- suchThat arbitrary (\n' -> isJust $ findQueryExpression n' expr)
                    let exprAt = fromJust $ findQueryExpression n expr
                        replaceResult = withQueryExpression n expr (const exprAt)
                    let propTest = expr == replaceResult
                    pure propTest
                )
            , testProperty
                "TagExpression"
                ( do
                    expr <-
                        ( ( mapK
                                ( (\(QCFreeMagma x) -> x) ::
                                    forall b. QCFreeMagma b -> FreeMagma b
                                )
                                . mapT
                                    ( coerce ::
                                        forall b. QCRingExpression b -> RingExpression b
                                    )
                                . runQCFreeCompoundExpression
                                . fmap (coerce :: QCDTerm QCPattern -> DTerm Pattern)
                          ) ::
                            QCFreeCompoundExpression
                                QCRingExpression
                                QCFreeMagma
                                (QCDTerm QCPattern) ->
                            TagQueryExpression
                        )
                            <$> resize 3 arbitrary ::
                            Gen TagQueryExpression
                    n <- suchThat arbitrary (\n' -> isJust $ findTagExpression n' expr)
                    let exprAt = fromJust $ findTagExpression n expr
                        replaceResult = normalize $ withTagExpression n expr (const exprAt)
                    let propTest = normalize expr == replaceResult
                    pure $
                        whenFail
                            ( do
                                print n
                                print exprAt
                                print expr
                                print replaceResult
                            )
                            propTest
                )
            ]
        ]