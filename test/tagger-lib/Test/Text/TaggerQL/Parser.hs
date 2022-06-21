{-# LANGUAGE OverloadedStrings #-}

module Test.Text.TaggerQL.Parser (
  queryParserTests,
) where

import Data.Tagger
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.AST
import Text.TaggerQL.Parser.Internal

queryParserTests :: TestTree
queryParserTests =
  testGroup
    "QueryParser Tests"
    [ testCase
        "NA"
        (assertFailure "Not Implemented")
    ]