{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module NewAST.Main (
  parserTests,
) where

import Data.Tagger
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.NewAST.AST
import Text.TaggerQL.NewAST.Parser

parserTests :: TestTree
parserTests =
  testGroup
    "Expression-based Parser Tests"
    [ testGroup
        "Tokens"
        [ testGroup
            "Criteria"
            ( ( \(a, s) ->
                  testCase
                    (T.unpack s)
                    ( assertEqual
                        ""
                        ( Right
                            ( Value . Nullary $ Term a s
                            )
                        )
                        (parseExpression (s <> "_"))
                    )
              )
                <$> [ (MetaDescriptorCriteria, "")
                    , (MetaDescriptorCriteria, "r.")
                    , (MetaDescriptorCriteria, "R.")
                    , (DescriptorCriteria, "d.")
                    , (DescriptorCriteria, "D.")
                    , (FilePatternCriteria, "p.")
                    , (FilePatternCriteria, "P.")
                    , (UntaggedCriteria, "u.")
                    , (UntaggedCriteria, "U.")
                    ]
            )
        , testGroup
            "Set Ops"
            ( ( \(a, s) ->
                  testCase
                    (T.unpack s)
                    ( assertEqual
                        ""
                        ( Right
                            ( Expression
                                (valnullR "_")
                                a
                                (valnullR "_")
                            )
                        )
                        (parseExpression ("_ " <> s <> " _"))
                    )
              )
                <$> [ (Union, "u|")
                    , (Union, "U|")
                    , (Intersect, "i|")
                    , (Intersect, "I|")
                    , (Difference, "d|")
                    , (Difference, "D|")
                    ]
            )
        ]
    , testCase
        "Default Left Associative"
        ( assertEqual
            ""
            ( Right
                ( Expression
                    ( Expression
                        ( Expression
                            (valnullR "a")
                            Intersect
                            (valnullR "b")
                        )
                        Intersect
                        (valnullR "c")
                    )
                    Intersect
                    (valnullR "d")
                )
            )
            (parseExpression "a i| b i| c i| d")
        )
    , testCase
        "Explicit Right Associative"
        ( assertEqual
            ""
            ( Right
                ( Expression
                    (valnullR "a")
                    Intersect
                    ( Expression
                        (valnullR "b")
                        Intersect
                        ( Expression
                            (valnullR "c")
                            Intersect
                            (valnullR "d")
                        )
                    )
                )
            )
            (parseExpression "a i| (b i| (c i| d))")
        )
    , testCase
        "Mixed Associativity"
        ( assertEqual
            ""
            ( Right
                ( Expression
                    ( Expression
                        ( Expression
                            (valnullR "a")
                            Intersect
                            (valnullR "b")
                        )
                        Intersect
                        ( Expression
                            (valnullR "c")
                            Intersect
                            ( Expression
                                (valnullR "d")
                                Intersect
                                (valnullR "e")
                            )
                        )
                    )
                    Intersect
                    (valnullR "f")
                )
            )
            (parseExpression "a i| b i| (c i| (d i| e)) i| f")
        )
    ]

valnullR :: T.Text -> Expression
valnullR = Value . Nullary . Term MetaDescriptorCriteria