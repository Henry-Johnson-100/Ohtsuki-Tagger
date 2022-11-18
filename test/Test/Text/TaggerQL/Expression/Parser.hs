{-# LANGUAGE OverloadedStrings #-}

module Test.Text.TaggerQL.Expression.Parser (
  parserTests,
) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Parser

parserTests :: TestTree
parserTests =
  testGroup
    "Parser Tests"
    [ testGroup
        "Misc Parser Tests"
        [ testGroup
            "patternParser"
            ( let parseP = parse patternParser ""
               in [ testCase "Pattern 0" (assertEqual "" (Right "a") (parseP "a"))
                  , testCase "Pattern 1" (assertEqual "" (Right "a") (parseP "\\a"))
                  , testCase "Pattern 2" (assertBool "" (isLeft (parseP "&")))
                  , testCase "Pattern 3" (assertEqual "" (Right "&") (parseP "\\&"))
                  , testCase "Pattern 4" (assertEqual "" (Right "aa") (parseP "aa"))
                  , testCase "Pattern 5" (assertEqual "" (Right "aa") (parseP "a\\a"))
                  , testCase "Pattern 6" (assertEqual "" (Right "\\") (parseP "\\\\"))
                  , testCase "Pattern 7" (assertBool "" (isLeft (parseP "a&a")))
                  , testCase "Pattern 8" (assertEqual "" (Right "a&a") (parseP "a\\&a"))
                  ]
            )
        ]
    ]