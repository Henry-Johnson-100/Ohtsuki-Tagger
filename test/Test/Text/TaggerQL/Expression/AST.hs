{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test.Text.TaggerQL.Expression.AST (
    astTests,
) where

import Control.Monad ((>=>))
import qualified Data.Foldable as F
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.TaggerQL.Expression.AST

-- change this at some point
instance Arbitrary Pattern where
    arbitrary :: Gen Pattern
    arbitrary = Pattern . T.pack <$> suchThat arbitrary (not . null)

instance Arbitrary a => Arbitrary (DTerm a) where
    arbitrary :: Arbitrary a => Gen (DTerm a)
    arbitrary = oneof (pure <$> [DTerm, DMetaTerm]) <*> arbitrary

instance Function a => Function (DTerm a)

instance Arbitrary a => Arbitrary (RingExpression a) where
    arbitrary :: Arbitrary a => Gen (RingExpression a)
    arbitrary = sized sizedRing
      where
        sizedRing n
            | n <= 0 = Ring <$> arbitrary
            | otherwise =
                oneof
                    [ (:+) <$> sizedRing (n `div` 2) <*> sizedRing (n `div` 2)
                    , (:*) <$> sizedRing (n `div` 2) <*> sizedRing (n `div` 2)
                    , (:-) <$> sizedRing (n `div` 2) <*> sizedRing (n `div` 2)
                    ]

instance Function a => Function (RingExpression a)

instance Arbitrary a => Arbitrary (MagmaExpression a) where
    arbitrary :: Arbitrary a => Gen (MagmaExpression a)
    arbitrary = do
        xs' <- arbitrary
        case xs' of
            (x : xs) -> pure $ F.foldl' over (pure x) xs
            _emptyList -> Magma <$> arbitrary

instance Function a => Function (MagmaExpression a)

instance Arbitrary a => Arbitrary (TagExpression a) where
    arbitrary :: Arbitrary a => Gen (TagExpression a)
    arbitrary = sized sizedExpr
      where
        sizedExpr n
            | n <= 0 = TagValue <$> arbitrary
            | otherwise =
                let tagRing = TagRing <$> resize (n `div` 2) arbitrary
                    tagMagma = TagMagma <$> resize (n `div` 2) arbitrary
                 in oneof [tagRing, tagMagma]

instance Function a => Function (TagExpression a)

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
                    f <- arbitrary :: Gen (Int -> RingExpression Int)
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- arbitrary :: Gen (RingExpression Int)
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
                                Gen (Fun Int (RingExpression Int))
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
                    f <- arbitrary :: Gen (Int -> MagmaExpression Int)
                    i <- arbitrary
                    let testProp = (pure i >>= f) == f i
                    pure testProp
                )
            , testProperty
                "Right Monad Identity"
                ( do
                    re <- arbitrary :: Gen (MagmaExpression Int)
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
                                Gen (Fun Int (MagmaExpression Int))
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
        ]
