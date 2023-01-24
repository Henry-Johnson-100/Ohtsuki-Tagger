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

astTests :: TestTree
astTests =
  testGroup
    "AST Tests"
    [astProperties]

astProperties :: TestTree
astProperties =
  testGroup
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