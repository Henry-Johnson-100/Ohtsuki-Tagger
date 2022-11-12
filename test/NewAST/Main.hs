{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

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
                            ( Value . Nullary $ Term a "_"
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
    , testCase
        "Implicit Intersection Expressions"
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
            (parseExpression "a b c d")
        )
    , testCase
        "Single Distribution"
        ( assertEqual
            ""
            ( Right
                ( Value
                    ( NAry
                        ( Term MetaDescriptorCriteria "a"
                            :<- Bottom (Term MetaDescriptorCriteria "b")
                        )
                    )
                )
            )
            (parseExpression "a(b)")
        )
    , testCase
        "Binary Distribution"
        ( assertEqual
            ""
            ( Right
                ( Expression
                    ( Value
                        ( NAry
                            ( Term MetaDescriptorCriteria "a"
                                :<- Bottom (Term MetaDescriptorCriteria "b")
                            )
                        )
                    )
                    Intersect
                    ( Value
                        ( NAry
                            ( Term MetaDescriptorCriteria "a"
                                :<- Bottom (Term MetaDescriptorCriteria "c")
                            )
                        )
                    )
                )
            )
            (parseExpression "a(b i| c)")
        )
    , testCase
        "Distributive Property In-Situ 1"
        ( assertEqual
            ""
            (parseExpression "a(b) i| a(c)")
            (parseExpression "a(b i| c)")
        )
    , testCase
        "Distributivity does not affect associativity 1"
        ( assertEqual
            ""
            (parseExpression "a(b) i| a(c) i| a(d)")
            (parseExpression "a(b i| c i| d)")
        )
    , testCase
        "Distributivity does not affect associativity 2"
        ( assertEqual
            ""
            (parseExpression "a(b) i| (a(c) i| (a(d) i| a(e)))")
            (parseExpression "a(b i| (c i| (d i| e)))")
        )
    , testCase
        "Distributivity over Mixed Associativity"
        ( assertEqual
            ""
            ( parseExpression
                "a(b) i| a(c) i| (a(d) i| (a(e) i| a(f)) i| a(g)) i| a(h)"
            )
            (parseExpression "a(b i| c i| (d i| (e i| f) i| g) i| h)")
        )
    , testCase
        "Distributivity over Implicit SetOp"
        ( assertEqual
            ""
            (parseExpression "a(b) a(c) i| (a(d) i| (a(e) a(f)) a(g)) a(h)")
            (parseExpression "a(b c i| (d i| (e f) g) h)")
        )
    , testCase
        "Implicit SetOps defer to Implicit Distribution"
        ( assertEqual
            ""
            ( Right
                ( Expression
                    ( Expression
                        (Value . NAry $ trm "a" :<- Bottom (trm "b"))
                        Intersect
                        ( Expression
                            ( Expression
                                (Value . NAry $ trm "a" :<- (trm "c" :<- (trm "d" :<- Bottom (trm "e"))))
                                Intersect
                                (Value . NAry $ trm "a" :<- (trm "c" :<- (trm "d" :<- Bottom (trm "f"))))
                            )
                            Intersect
                            (Value . NAry $ trm "a" :<- (trm "c" :<- Bottom (trm "g")))
                        )
                    )
                    Intersect
                    (Value . NAry $ trm "a" :<- Bottom (trm "h"))
                )
            )
            ( parseExpression "a(b c(d(e f) g) h)"
            {-
            desugaring and back again:

            a(b c(d(e f) g) h)
            distribute d
            a(b c((d(e) d(f)) g) h)
            distribute c
            a(b (c(d(e) d(f)) c(g)) h)
            distribute c
            a(b ((c(d(e)) c(d(f))) c(g)) h)
            distribute a
            (a(b) a((c(d(e)) c(d(f))) c(g)) a(h))
            distribute a
            (a(b) (a(c(d(e)) c(d(f))) a(c(g))) a(h))
            distribute a
            (a(b) ((a(c(d(e))) a(c(d(f)))) a(c(g))) a(h))
            make setop explicit
            (a(b) I| ((a(c(d(e))) I| a(c(d(f)))) I| a(c(g))) I| a(h))
            substitute terms: v w x y z
            (v I| ((w I| x) I| y) I| z)
              where
                v = a(b)
                w = a(c(d(e)))
                x = a(c(d(f)))
                y = a(c(g))
                z = a(h)

            add explicit SetOps to original query
            a(b I| c(d(e I| f) I| g) I| h)
            -}
            )
        )
    , testCase
        "Desugared previous test case"
        ( assertEqual
            ""
            ( Right
                ( Expression
                    ( Expression
                        (Value . NAry $ trm "a" :<- Bottom (trm "b"))
                        Intersect
                        ( Expression
                            ( Expression
                                (Value . NAry $ trm "a" :<- (trm "c" :<- (trm "d" :<- Bottom (trm "e"))))
                                Intersect
                                (Value . NAry $ trm "a" :<- (trm "c" :<- (trm "d" :<- Bottom (trm "f"))))
                            )
                            Intersect
                            (Value . NAry $ trm "a" :<- (trm "c" :<- Bottom (trm "g")))
                        )
                    )
                    Intersect
                    (Value . NAry $ trm "a" :<- Bottom (trm "h"))
                )
            )
            (parseExpression "a(b I| c(d(e I| f) I| g) I| h)")
        )
    , testCase
        "Implicit SetOps defer to Implicit Distribution - In-Situ"
        ( assertEqual
            ""
            (parseExpression "(a(b) I| ((a(c(d(e))) I| a(c(d(f)))) I| a(c(g))) I| a(h))")
            (parseExpression "a(b c(d(e f) g) h)")
        )
    ]

valnullR :: T.Text -> Expression
valnullR = Value . Nullary . Term MetaDescriptorCriteria

trm :: T.Text -> Term
trm = Term MetaDescriptorCriteria
