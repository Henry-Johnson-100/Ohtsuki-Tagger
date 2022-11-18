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
    , testGroup
        "Term Parser Tests"
        [ testGroup
            "fileTermParser"
            ( let parseFT = parse fileTermParser ""
               in [ testCase
                      "Requires preceding token"
                      ( assertEqual
                          "A file term must be preceded by \"p.\""
                          (Right "hello")
                          (parseFT "p.hello")
                      )
                  , testCase
                      "Preceding token is case insensitive"
                      (assertEqual "" (Right "hello") (parseFT "P.hello"))
                  , testCase
                      "Fails without preceding token"
                      ( assertBool
                          "A file term must be preceded by \"p.\""
                          (isLeft (parseFT "hello"))
                      )
                  ]
            )
        , testGroup
            "tagTermParser"
            ( let parseTT = parse tagTermParser ""
               in [ testCase
                      "MetaDescriptor can be preceded by token"
                      ( assertEqual
                          "A MetaDescriptor pattern can be preceded by \'r.\'"
                          (Right (MetaDescriptorTerm "hello"))
                          (parseTT "r.hello")
                      )
                  , testCase
                      "A Descriptor must be preceded by a token"
                      ( assertEqual
                          "A Descriptor pattern must be preceded by \'d.\'"
                          (Right (DescriptorTerm "hello"))
                          (parseTT "d.hello")
                      )
                  , testCase
                      "A MetaDescriptor does not have to be preceded by a pattern"
                      ( assertEqual
                          ""
                          (Right (MetaDescriptorTerm "hello"))
                          (parseTT "hello")
                      )
                  ]
            )
        ]
    ]