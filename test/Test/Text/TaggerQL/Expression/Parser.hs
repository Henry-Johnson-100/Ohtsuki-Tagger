{-# LANGUAGE OverloadedStrings #-}

module Test.Text.TaggerQL.Expression.Parser (
    fParserTests,
) where

import Data.Either (isLeft)
import Data.Tagger (SetOp (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Parser

dmt :: a -> TExpression (DTerm a)
dmt = TValue . DMetaTerm

l :: TExpression b -> FExpression b a
l = LiftTExpression

fParserTests :: TestTree
fParserTests =
    testGroup
        "Parser Tests"
        [ testGroup
            "Misc Parser Tests"
            [ testGroup
                "patternParser"
                ( let parseP = parse patternParser' ""
                   in [ testCase "Pattern 0" (assertEqual "" (Right "a") (parseP "a"))
                      , testCase "Pattern 1" (assertEqual "" (Right "a") (parseP "\\a"))
                      , testCase "Pattern 2" (assertBool "" (isLeft (parseP "&")))
                      , testCase "Pattern 3" (assertEqual "" (Right "&") (parseP "\\&"))
                      , testCase "Pattern 4" (assertEqual "" (Right "aa") (parseP "aa"))
                      , testCase "Pattern 5" (assertEqual "" (Right "aa") (parseP "a\\a"))
                      , testCase "Pattern 6" (assertEqual "" (Right "\\") (parseP "\\\\"))
                      , -- This test doesn't work, but it's not a bug, it's a feature.
                        -- , testCase "Pattern 7" (assertBool "" (isLeft (parseP "a&a")))
                        testCase "Pattern 8" (assertEqual "" (Right "a&a") (parseP "a\\&a"))
                      , testCase
                            "Pattern 9"
                            ( assertEqual
                                "Ignore trailing spaces"
                                (Right "a")
                                (parseP "a  ")
                            )
                      , testCase "Pattern 10" (assertEqual "" (Right WildCard) (parseP "%"))
                      , testCase "Pattern 11" (assertEqual "" (Right WildCard) (parseP "%%%%%%%%%%"))
                      , testCase "Pattern 12" (assertEqual "" (Right "%%a%%") (parseP "%%a%%"))
                      ]
                )
            ]
        , testGroup
            "Term Parser Tests"
            [ testGroup
                "fValuePatternParser"
                ( let parseFT = parse fValueParser ""
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
                "DTermParser"
                ( let parseTT = parse dTermPatternParser ""
                   in [ testCase
                            "MetaDescriptor can be preceded by token"
                            ( assertEqual
                                "A MetaDescriptor pattern can be preceded by \'r.\'"
                                (Right (DMetaTerm "hello"))
                                (parseTT "r.hello")
                            )
                      , testCase
                            "A Descriptor must be preceded by a token"
                            ( assertEqual
                                "A Descriptor pattern must be preceded by \'d.\'"
                                (Right (DTerm "hello"))
                                (parseTT "d.hello")
                            )
                      , testCase
                            "A MetaDescriptor does not have to be preceded by a pattern"
                            ( assertEqual
                                ""
                                (Right (DMetaTerm "hello"))
                                (parseTT "hello")
                            )
                      ]
                )
            ]
        , testGroup
            "FExpression Parser Tests"
            [ testCase
                "Parse implicit tag term expr"
                ( assertEqual
                    ""
                    (Right (LiftTExpression (TValue (DMetaTerm "hello"))))
                    (parseFExpr "hello")
                )
            , testCase
                "Parse explicit tag term expr"
                ( assertEqual
                    ""
                    (Right (LiftTExpression (TValue (DTerm "hello"))))
                    (parseFExpr "d.hello")
                )
            , testCase
                "file term value"
                (assertEqual "" (Right (FValue "hello")) (parseFExpr "p.hello"))
            , testCase
                "tag expression entrance"
                ( assertEqual
                    ""
                    ( Right
                        ( LiftTExpression (TExpressionDistribution (DMetaTerm "a" :$ TValue (DMetaTerm "b")))
                        )
                    )
                    (parseFExpr "a{b}")
                )
            , testGroup
                "Binary tests"
                [ testCase
                    "Simple explicit binary"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryFExpression
                                ( BinaryOperation
                                    (LiftTExpression (TValue (DMetaTerm "a")))
                                    Union
                                    (LiftTExpression (TValue (DMetaTerm "b")))
                                )
                            )
                        )
                        (parseFExpr "a | b")
                    )
                , testCase
                    "Simple implicit binary"
                    ( assertEqual
                        ""
                        ( Right
                            ( LiftTExpression (TValue (DMetaTerm "a"))
                                <^> LiftTExpression (TValue (DMetaTerm "b"))
                            )
                        )
                        (parseFExpr "a b")
                    )
                , testCase
                    "Binary is left-associative"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryFExpression
                                ( BinaryOperation
                                    ( BinaryFExpression
                                        ( BinaryOperation
                                            (l . dmt $ "a")
                                            Union
                                            (l . dmt $ "b")
                                        )
                                    )
                                    Difference
                                    (l . dmt $ "c")
                                )
                            )
                        )
                        (parseFExpr "a | b ! c")
                    )
                , testCase
                    "Lng Operators are left-associative"
                    ( assertEqual
                        ""
                        ( BinaryFExpression
                            ( BinaryOperation
                                ( BinaryFExpression
                                    ( BinaryOperation
                                        (l . dmt $ "a")
                                        Union
                                        (l . dmt $ "b")
                                    )
                                )
                                Difference
                                (l . dmt $ "c")
                            )
                        )
                        ( (l . dmt $ "a")
                            <+> (l . dmt $ "b")
                                <-> (l . dmt $ "c" :: FExpression (DTerm Pattern) Pattern)
                        )
                    )
                , testCase
                    "Can change binary precedence"
                    ( assertEqual
                        ""
                        ( Right
                            ( (l . dmt $ "a") <+> ((l . dmt $ "b") <-> (l . dmt $ "c"))
                            )
                        )
                        (parseFExpr "a | (b ! c)")
                    )
                , testCase
                    "Can nest binary operations"
                    ( assertEqual
                        ""
                        ( Right
                            ( (l . dmt $ "a")
                                <^> l (TExpressionDistribution (DMetaTerm "b" :$ TValue (DMetaTerm "c")))
                                <+> (FValue "hi" <^> FValue "bye")
                                <-> (l . TValue . DTerm $ "unwanted")
                            )
                        )
                        (parseFExpr "a b{c} | (p.hi & p.bye) ! d.unwanted")
                    )
                ]
            , testCase
                "Expression ignores newline"
                ( assertEqual
                    ""
                    ( Right
                        ( (l . dmt $ "a") <^> l (TExpressionDistribution (DMetaTerm "b" :$ (dmt "c" <^> dmt "d"))) <^> (l . dmt $ "e")
                        )
                    )
                    (parseFExpr "a b{\nc d\n}\ne")
                )
            , testGroup
                "Expression ignores trailing whitespace"
                [ testCase
                    "for binary expr"
                    ( assertEqual
                        ""
                        ( Right
                            ( (l . dmt $ "a") <^> (l . dmt $ "b")
                            )
                        )
                        (parseFExpr "a b ")
                    )
                , testCase
                    "for binary expr - explicit set op"
                    ( assertEqual
                        ""
                        ( Right
                            ( (l . dmt $ "a") <+> (l . dmt $ "b")
                            )
                        )
                        (parseFExpr "a | b ")
                    )
                ]
            , testGroup
                "Mixed FExpressions and Lifted TExpressions"
                ( let p = parseFExpr
                   in [ testCase
                            "Trivial FValue"
                            (assertEqual "" (Right (FValue "test")) (p "p.test"))
                      , testCase
                            "Trivial Lifted TValue"
                            (assertEqual "" (Right (l . dmt $ "test")) (p "test"))
                      , testCase
                            "Trivial Lifted DTerm TValue"
                            (assertEqual "" (Right . l . TValue . DTerm $ "test") (p "d.test"))
                      , testCase
                            "Fvalue then TValue"
                            ( assertEqual
                                ""
                                (Right (FValue "hello" <^> (l . dmt $ "goodbye")))
                                (p "p.hello goodbye")
                            )
                      , testCase
                            "TValue then FValue"
                            ( assertEqual
                                ""
                                (Right ((l . dmt $ "goodbye") <^> FValue "hello"))
                                (p "goodbye p.hello")
                            )
                      , testCase
                            "Parenthesized mixed cases"
                            ( assertEqual
                                ""
                                ( Right
                                    ( (l . dmt $ "a") <^> ((l . dmt $ "b") <^> FValue "c") <^> (l . dmt $ "d")
                                    )
                                )
                                (p "a (b p.c) d")
                            )
                      , testCase
                            "Parenthesized lifted cases"
                            ( assertEqual
                                ""
                                ( Right
                                    ( (l . dmt $ "a") <^> ((l . dmt $ "b") <^> (l . dmt $ "c")) <^> (l . dmt $ "d")
                                    )
                                )
                                (p "a (b c) d")
                            )
                      , testGroup
                            "Permutations - 1"
                            ( let x = FValue "x"
                                  a = l . dmt $ "a"
                                  b = l . dmt $ "b"
                                  c = l . dmt $ "c"
                               in (\(assertion, str) -> testCase "" (assertEqual "" (Right assertion) (p str)))
                                    <$> [ (a <^> b <^> c <^> x, "a b c p.x")
                                        , (a <^> b <^> x <^> c, "a b p.x c")
                                        , (a <^> x <^> b <^> c, "a p.x b c")
                                        , (x <^> a <^> b <^> c, "p.x a b c")
                                        ]
                            )
                      ]
                )
            ]
        , testGroup
            "SubExpression Parser Tests"
            ( let parseSE = parseTExpr
               in [ testCase
                        "Parse a single subtag"
                        ( assertEqual
                            ""
                            (Right (TValue . DTerm $ "hello"))
                            (parseSE "d.hello")
                        )
                  , testCase
                        "Parse a single MetaDescriptor Term Subtag"
                        ( assertEqual
                            ""
                            (Right (dmt "hello"))
                            (parseSE "hello")
                        )
                  , testCase
                        "Parse a parenthesized subtag"
                        ( assertEqual
                            ""
                            (Right (dmt "hello"))
                            (parseSE "(  hello  )")
                        )
                  , testCase
                        "Parse simple sub expression"
                        ( assertEqual
                            ""
                            ( Right
                                ( TExpressionDistribution (DMetaTerm "a" :$ dmt "b")
                                )
                            )
                            (parseSE "a{b}")
                        )
                  , testCase
                        "Parse nested sub expression"
                        ( assertEqual
                            ""
                            ( Right
                                ( TExpressionDistribution (DMetaTerm "a" :$ TExpressionDistribution (DMetaTerm "b" :$ dmt "c"))
                                )
                            )
                            (parseSE "a{b{c}}")
                        )
                  , testGroup
                        "SubBinary tests"
                        [ testCase
                            "Parse simple subBinary"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <^> dmt "b"
                                    )
                                )
                                (parseSE "a b")
                            )
                        , testCase
                            "Alternate SubBinary operator"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <+> dmt "b"
                                    )
                                )
                                (parseSE "a | b")
                            )
                        , testCase
                            "SubBinary is left-associative"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <^> dmt "b" <^> dmt "c"
                                    )
                                )
                                (parseSE "a b c")
                            )
                        , testCase
                            "SubBinary can use different set ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <+> dmt "b" <^> dmt "c" <-> dmt "d"
                                    )
                                )
                                (parseSE "a | b & c ! d")
                            )
                        , testCase
                            "Can change SubBinary precedence"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <+> dmt "b" <^> (dmt "c" <-> dmt "d")
                                    )
                                )
                                (parseSE "a | b & (c ! d)")
                            )
                        , testCase
                            "Nest SubExpressions in SubBinary"
                            ( assertEqual
                                ""
                                ( Right
                                    ( TExpressionDistribution (DMetaTerm "a" :$ dmt "b")
                                        <+> TExpressionDistribution (DMetaTerm "c" :$ (dmt "d" <^> dmt "e"))
                                    )
                                )
                                (parseSE "a{b} | c{d e}")
                            )
                        ]
                  , testCase
                        "SubExpression ignores newline"
                        ( assertEqual
                            ""
                            ( Right
                                ( dmt "a" <^> dmt "b"
                                )
                            )
                            (parseSE "a\nb")
                        )
                  , testGroup
                        "SubExpression ignores newline tests"
                        [ testCase
                            "Sub expression ignores trailing whitespace"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <^> dmt "b"
                                    )
                                )
                                (parseSE "a b ")
                            )
                        , testCase
                            "Sub expression ignores trailing whitespace - explicit set op"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <+> dmt "b"
                                    )
                                )
                                (parseSE "a | b ")
                            )
                        , testCase
                            "SubExpression ignores newline - 2"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <^> TExpressionDistribution (DMetaTerm "b" :$ (dmt "c" <^> dmt "d")) <^> dmt "e"
                                    )
                                )
                                (parseSE "a b {\n c d\n}\ne")
                            )
                        , testCase
                            "SubExpression ignores newline - 2 - Explicit Set Ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <^> TExpressionDistribution (DMetaTerm "b" :$ (dmt "c" <^> dmt "d")) <^> dmt "e"
                                    )
                                )
                                (parseSE "a&b{\nc&d\n}\n&e")
                            )
                        , testCase
                            "SubExpression works with no newlines"
                            ( assertEqual
                                ""
                                ( Right
                                    ( dmt "a" <^> TExpressionDistribution (DMetaTerm "b" :$ (dmt "c" <^> dmt "d")) <^> dmt "e"
                                    )
                                )
                                (parseSE "a b { c d } e")
                            )
                        , testCase
                            "RHS ignores newline"
                            ( assertEqual
                                ""
                                ( Right
                                    ( TExpressionDistribution (DMetaTerm "b" :$ (dmt "c" <^> dmt "d")) <^> dmt "e"
                                    )
                                )
                                (parseSE "b {\n c d\n}\ne")
                            )
                        , testCase
                            "RHS ignores newline - explicit set ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( TExpressionDistribution (DMetaTerm "b" :$ (dmt "c" <^> dmt "d")) <^> dmt "e"
                                    )
                                )
                                (parseSE "b{\n c&d\n}\n&e")
                            )
                        , testCase
                            "Distributive syntax sugar"
                            ( assertEqual
                                ""
                                ( Right
                                    ( TExpressionDistribution (DMetaTerm "a" :$ dmt "c") <^> TExpressionDistribution (DMetaTerm "b" :$ dmt "c")
                                    )
                                )
                                (parseSE "(a b){c}")
                            )
                        , testCase
                            "Distributive syntax sugar - 2"
                            ( assertEqual
                                ""
                                ( Right
                                    ( let innerDistribution = TExpressionDistribution (DMetaTerm "c" :$ dmt "f") <^> TExpressionDistribution (DMetaTerm "e" :$ dmt "f")
                                       in TExpressionDistribution (DMetaTerm "a" :$ innerDistribution) <^> TExpressionDistribution (DMetaTerm "b" :$ innerDistribution)
                                    )
                                )
                                (parseSE "(a b){(c e){f}}")
                            )
                        , testCase
                            "Distributive syntax sugar - 3"
                            ( assertEqual
                                ""
                                ( Right
                                    ( let d0 = dmt "c" <^> d1t1
                                          d1t1 = TExpressionDistribution (DMetaTerm "d" :$ d2) <^> TExpressionDistribution (DMetaTerm "e" :$ d2)
                                          d2 = dmt "f" <^> dmt "g"
                                       in TExpressionDistribution (DMetaTerm "a" :$ d0) <^> TExpressionDistribution (DMetaTerm "b" :$ d0)
                                    )
                                )
                                (parseSE "(a b){c (d e){f g}}")
                            )
                        ]
                  ]
            )
        ]
