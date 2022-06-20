module Test.Database.Tagger (
  taggerDatabaseTests,
) where

import qualified Data.HashSet as HS
import Database.Tagger
import Test.Tasty
import Test.Tasty.HUnit

taggerDatabaseTests :: TestTree
taggerDatabaseTests = testCase "Database.Tagger Tests" $ assertFailure "Not implemented"