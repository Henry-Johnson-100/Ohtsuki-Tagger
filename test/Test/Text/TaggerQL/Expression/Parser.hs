{-# LANGUAGE OverloadedStrings #-}

module Test.Text.TaggerQL.Expression.Parser (
  parserTests,
) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Parser

parserTests :: TestTree
parserTests = testGroup "Parser Tests" []