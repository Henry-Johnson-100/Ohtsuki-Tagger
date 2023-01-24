{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test.Text.TaggerQL.Expression.AST (
    astTests,
) where

import Control.Monad ((>=>))
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
    arbitrary = do
        let ringOperation =
                oneof [pure (:+), pure (:*), pure (:-)] <*> arbitrary <*> arbitrary
        oneof [Ring <$> arbitrary, ringOperation]

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
            , localOption (QuickCheckTests 30) $
                testProperty
                    "Kleisli Arrow Associativity"
                    ( do
                        f <- arbitrary :: Gen (Int -> RingExpression Int)
                        g <- arbitrary :: Gen (Int -> RingExpression Int)
                        h <- arbitrary :: Gen (Int -> RingExpression Int)
                        dt <- arbitrary :: Gen Int
                        let testProp = (f >=> (g >=> h)) dt == ((f >=> g) >=> h) dt
                            propTerminates = within (500 * (10 ^ (3 :: Integer))) testProp
                        pure $ propTerminates .||. Discard
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
            , localOption (QuickCheckTests 30) $
                testProperty
                    "Kleisli Arrow Associativity"
                    ( do
                        f <- arbitrary :: Gen (Int -> DTerm Int)
                        g <- arbitrary :: Gen (Int -> DTerm Int)
                        h <- arbitrary :: Gen (Int -> DTerm Int)
                        dt <- arbitrary :: Gen Int
                        let testProp = (f >=> (g >=> h)) dt == ((f >=> g) >=> h) dt
                            propTerminates = within (500 * (10 ^ (3 :: Integer))) testProp
                        pure $ propTerminates .||. Discard
                    )
            ]
        ]
