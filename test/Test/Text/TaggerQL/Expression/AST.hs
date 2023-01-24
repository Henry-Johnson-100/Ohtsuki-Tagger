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
    arbitrary = do
        constructor <- oneof [pure DTerm, pure DMetaTerm]
        value <- arbitrary
        pure . constructor $ value

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

instance Arbitrary a => Arbitrary (MagmaExpression a) where
    arbitrary :: Arbitrary a => Gen (MagmaExpression a)
    arbitrary = do
        xs' <- arbitrary
        case xs' of
            (x : xs) -> pure $ F.foldl' over (pure x) xs
            _emptyList -> Magma <$> arbitrary

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
                    let f n = pure (n `div` 2) :+ pure (n * n)
                        g n = pure (n - (n * (-3))) :* pure n
                        h n = pure n :- (pure (n + 23) :* pure (n * 1001))
                    dt <- arbitrary :: Gen Int
                    let testProp = (f >=> (g >=> h)) dt == ((f >=> g) >=> h) dt
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
                    f <- arbitrary :: Gen (Int -> MagmaExpression Int)
                    g <- arbitrary :: Gen (Int -> MagmaExpression Int)
                    h <- arbitrary :: Gen (Int -> MagmaExpression Int)
                    dt <- arbitrary :: Gen Int
                    let testProp = (f >=> (g >=> h)) dt == ((f >=> g) >=> h) dt
                        propTerminates = within (500 * (10 ^ (3 :: Integer))) testProp
                    pure propTerminates
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
                    let f n = pure (n `div` 2)
                        g n = DTerm (n - (n * (-3)))
                        h n = DMetaTerm (n * 1001)
                    dt <- arbitrary :: Gen Int
                    let testProp = (f >=> (g >=> h)) dt == ((f >=> g) >=> h) dt
                    pure testProp
                )
            ]
        ]
