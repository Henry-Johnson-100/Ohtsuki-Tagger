module Test.Text.TaggerQL.Expression.AST (
    astTests,
) where

import Control.Monad ((>=>))
import Test.Resources
import Test.Tasty
import Test.Tasty.QuickCheck

astTests :: TestTree
astTests =
    testGroup
        "AST Tests"
        [astProperties]

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
            "MagmaExpression Properties"
            [ testProperty
                "Left Monad Identity"
                ( do
                    f <- arbitrary :: Gen (Int -> QCMagmaExpression Int)
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- arbitrary :: Gen (QCMagmaExpression Int)
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
                                Gen (Fun Int (QCMagmaExpression Int))
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
                    f <- resize 3 arbitrary :: Gen (Int -> QCTagExpression Int)
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- resize 3 arbitrary :: Gen (QCTagExpression Int)
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
                                Gen (Fun Int (QCTagExpression Int))
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
