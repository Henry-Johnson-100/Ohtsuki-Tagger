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
    [ testGroup
        "term_parser_tests"
        [ testCase
            "escape_unallowed_characters"
            ( assertEqual
                "Unallowed characters should be escapable"
                (Right $ TermPattern DescriptorCriteria ":|")
                (parse termParser "d.:\\|")
            )
        ]
    ]