module Test.Text.QueryParser (
  queryParserTests,
) where

import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Text.QueryParser.Internal

queryParserTests :: TestTree
queryParserTests =
  testGroup
    "QueryParser Tests"
    [ testCase "NA" (assertFailure "Not Implemented")
    ]