{-# LANGUAGE OverloadedStrings #-}

module NewAST.Main (
  parserTests,
) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.NewAST.AST
import Text.TaggerQL.NewAST.Parser

parserTests =
  testGroup
    "Expression-based Parser Tests"
    []