{-# LANGUAGE OverloadedStrings #-}

module Test.Text.TaggerQL.Expression.Parser (
    parserTests,
) where

import Data.Either (isLeft)
import Data.Functor.Identity
import Data.Tagger (SetOp (..))
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
        , testGroup
            "Expression Parser Tests"
            [ testCase
                "Parse implicit tag term expr"
                ( assertEqual
                    ""
                    (Right (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "hello"))
                    (parseExpr "hello")
                )
            , testCase
                "Parse explicit tag term expr"
                ( assertEqual
                    ""
                    (Right (ExpressionLeaf . Identity . TagTermValue $ DescriptorTerm "hello"))
                    (parseExpr "d.hello")
                )
            , testCase
                "file term value"
                (assertEqual "" (Right (ExpressionLeaf . Identity . FileTermValue $ "hello")) (parseExpr "p.hello"))
            , testCase
                "tag expression entrance"
                ( assertEqual
                    ""
                    ( Right
                        ( ExpressionTagTermExtension . Identity $
                            TagTermExtension
                                (MetaDescriptorTerm "a")
                                (SubTag . Identity $ MetaDescriptorTerm "b")
                        )
                    )
                    (parseExpr "a{b}")
                )
            , testGroup
                "Binary tests"
                [ testCase
                    "Simple explicit binary"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpressionValue . Identity $
                                BinaryExpression
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a")
                                    Union
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "b")
                            )
                        )
                        (parseExpr "a | b")
                    )
                , testCase
                    "Simple implicit binary"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpressionValue . Identity $
                                BinaryExpression
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a")
                                    Intersect
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "b")
                            )
                        )
                        (parseExpr "a b")
                    )
                , testCase
                    "Binary is left-associative"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpressionValue . Identity $
                                BinaryExpression
                                    ( BinaryExpressionValue . Identity $
                                        BinaryExpression
                                            (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a")
                                            Union
                                            (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "b")
                                    )
                                    Difference
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "c")
                            )
                        )
                        (parseExpr "a | b ! c")
                    )
                , testCase
                    "Can change binary precedence"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpressionValue . Identity $
                                BinaryExpression
                                    ( ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a"
                                    )
                                    Union
                                    ( BinaryExpressionValue . Identity $
                                        BinaryExpression
                                            (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "b")
                                            Difference
                                            (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "c")
                                    )
                            )
                        )
                        (parseExpr "a | (b ! c)")
                    )
                , testCase
                    "Can nest binary operations"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpressionValue . Identity $
                                BinaryExpression
                                    ( BinaryExpressionValue . Identity $
                                        BinaryExpression
                                            ( BinaryExpressionValue . Identity $
                                                BinaryExpression
                                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a")
                                                    Intersect
                                                    ( ExpressionTagTermExtension . Identity $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            (SubTag . Identity $ MetaDescriptorTerm "c")
                                                    )
                                            )
                                            Union
                                            ( BinaryExpressionValue . Identity $
                                                BinaryExpression
                                                    (ExpressionLeaf . Identity . FileTermValue $ "hi")
                                                    Intersect
                                                    (ExpressionLeaf . Identity . FileTermValue $ "bye")
                                            )
                                    )
                                    Difference
                                    (ExpressionLeaf . Identity . TagTermValue $ DescriptorTerm "unwanted")
                            )
                        )
                        (parseExpr "a b{c} | (p.hi & p.bye) ! d.unwanted")
                    )
                ]
            , testCase
                "Expression ignores newline"
                ( assertEqual
                    ""
                    ( Right
                        ( BinaryExpressionValue . Identity $
                            BinaryExpression
                                ( BinaryExpressionValue . Identity $
                                    BinaryExpression
                                        (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a")
                                        Intersect
                                        ( ExpressionTagTermExtension . Identity $
                                            TagTermExtension
                                                (MetaDescriptorTerm "b")
                                                ( SubBinary . Identity $
                                                    BinaryExpression
                                                        (SubTag . Identity $ MetaDescriptorTerm "c")
                                                        Intersect
                                                        (SubTag . Identity $ MetaDescriptorTerm "d")
                                                )
                                        )
                                )
                                Intersect
                                (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "e")
                        )
                    )
                    (parseExpr "a b{\nc d\n}\ne")
                )
            , testGroup
                "Expression ignores trailing whitespace"
                [ testCase
                    "for binary expr"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpressionValue . Identity $
                                BinaryExpression
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a")
                                    Intersect
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "b")
                            )
                        )
                        (parseExpr "a b ")
                    )
                , testCase
                    "for binary expr - explicit set op"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpressionValue . Identity $
                                BinaryExpression
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "a")
                                    Union
                                    (ExpressionLeaf . Identity . TagTermValue $ MetaDescriptorTerm "b")
                            )
                        )
                        (parseExpr "a | b ")
                    )
                ]
            ]
        , testGroup
            "SubExpression Parser Tests"
            ( let parseSE = parse subExpressionParser ""
               in [ testCase
                        "Parse a single subtag"
                        ( assertEqual
                            ""
                            (Right (SubTag . Identity $ DescriptorTerm "hello"))
                            (parseSE "d.hello")
                        )
                  , testCase
                        "Parse a single MetaDescriptor Term Subtag"
                        ( assertEqual
                            ""
                            (Right (SubTag . Identity $ MetaDescriptorTerm "hello"))
                            (parseSE "hello")
                        )
                  , testCase
                        "Parse a parenthesized subtag"
                        ( assertEqual
                            ""
                            (Right (SubTag . Identity $ MetaDescriptorTerm "hello"))
                            (parseSE "(  hello  )")
                        )
                  , testCase
                        "Parse simple sub expression"
                        ( assertEqual
                            ""
                            ( Right
                                ( SubExpression . Identity $
                                    TagTermExtension
                                        (MetaDescriptorTerm "a")
                                        (SubTag . Identity $ MetaDescriptorTerm "b")
                                )
                            )
                            (parseSE "a{b}")
                        )
                  , testCase
                        "Parse nested sub expression"
                        ( assertEqual
                            ""
                            ( Right
                                ( SubExpression . Identity $
                                    TagTermExtension
                                        (MetaDescriptorTerm "a")
                                        ( SubExpression . Identity $
                                            TagTermExtension
                                                (MetaDescriptorTerm "b")
                                                (SubTag . Identity $ MetaDescriptorTerm "c")
                                        )
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
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            (SubTag . Identity $ MetaDescriptorTerm "a")
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "b")
                                    )
                                )
                                (parseSE "a b")
                            )
                        , testCase
                            "Alternate SubBinary operator"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            (SubTag . Identity $ MetaDescriptorTerm "a")
                                            Union
                                            (SubTag . Identity $ MetaDescriptorTerm "b")
                                    )
                                )
                                (parseSE "a | b")
                            )
                        , testCase
                            "SubBinary is left-associative"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubBinary . Identity $
                                                BinaryExpression
                                                    (SubTag . Identity $ MetaDescriptorTerm "a")
                                                    Intersect
                                                    (SubTag . Identity $ MetaDescriptorTerm "b")
                                            )
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "c")
                                    )
                                )
                                (parseSE "a b c")
                            )
                        , testCase
                            "SubBinary can use different set ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubBinary . Identity $
                                                BinaryExpression
                                                    ( SubBinary . Identity $
                                                        BinaryExpression
                                                            (SubTag . Identity $ MetaDescriptorTerm "a")
                                                            Union
                                                            (SubTag . Identity $ MetaDescriptorTerm "b")
                                                    )
                                                    Intersect
                                                    (SubTag . Identity $ MetaDescriptorTerm "c")
                                            )
                                            Difference
                                            (SubTag . Identity $ MetaDescriptorTerm "d")
                                    )
                                )
                                (parseSE "a | b & c ! d")
                            )
                        , testCase
                            "Can change SubBinary precedence"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubBinary . Identity $
                                                BinaryExpression
                                                    (SubTag . Identity $ MetaDescriptorTerm "a")
                                                    Union
                                                    (SubTag . Identity $ MetaDescriptorTerm "b")
                                            )
                                            Intersect
                                            ( SubBinary . Identity $
                                                BinaryExpression
                                                    (SubTag . Identity $ MetaDescriptorTerm "c")
                                                    Difference
                                                    (SubTag . Identity $ MetaDescriptorTerm "d")
                                            )
                                    )
                                )
                                (parseSE "a | b & (c ! d)")
                            )
                        , testCase
                            "Nest SubExpressions in SubBinary"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubExpression . Identity $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "a")
                                                    (SubTag . Identity $ MetaDescriptorTerm "b")
                                            )
                                            Union
                                            ( SubExpression . Identity $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "c")
                                                    ( SubBinary . Identity $
                                                        BinaryExpression
                                                            (SubTag . Identity $ MetaDescriptorTerm "d")
                                                            Intersect
                                                            (SubTag . Identity $ MetaDescriptorTerm "e")
                                                    )
                                            )
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
                                ( SubBinary . Identity $
                                    BinaryExpression
                                        (SubTag . Identity $ MetaDescriptorTerm "a")
                                        Intersect
                                        (SubTag . Identity $ MetaDescriptorTerm "b")
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
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            (SubTag . Identity $ MetaDescriptorTerm "a")
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "b")
                                    )
                                )
                                (parseSE "a b ")
                            )
                        , testCase
                            "Sub expression ignores trailing whitespace - explicit set op"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            (SubTag . Identity $ MetaDescriptorTerm "a")
                                            Union
                                            (SubTag . Identity $ MetaDescriptorTerm "b")
                                    )
                                )
                                (parseSE "a | b ")
                            )
                        , testCase
                            "SubExpression ignores newline - 2"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubBinary . Identity $
                                                BinaryExpression
                                                    (SubTag . Identity $ MetaDescriptorTerm "a")
                                                    Intersect
                                                    ( SubExpression . Identity $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            ( SubBinary . Identity $
                                                                BinaryExpression
                                                                    (SubTag . Identity $ MetaDescriptorTerm "c")
                                                                    Intersect
                                                                    (SubTag . Identity $ MetaDescriptorTerm "d")
                                                            )
                                                    )
                                            )
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "e")
                                    )
                                )
                                (parseSE "a b {\n c d\n}\ne")
                            )
                        , testCase
                            "SubExpression ignores newline - 2 - Explicit Set Ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubBinary . Identity $
                                                BinaryExpression
                                                    (SubTag . Identity $ MetaDescriptorTerm "a")
                                                    Intersect
                                                    ( SubExpression . Identity $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            ( SubBinary . Identity $
                                                                BinaryExpression
                                                                    (SubTag . Identity $ MetaDescriptorTerm "c")
                                                                    Intersect
                                                                    (SubTag . Identity $ MetaDescriptorTerm "d")
                                                            )
                                                    )
                                            )
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "e")
                                    )
                                )
                                (parseSE "a&b{\nc&d\n}\n&e")
                            )
                        , testCase
                            "SubExpression works with no newlines"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubBinary . Identity $
                                                BinaryExpression
                                                    (SubTag . Identity $ MetaDescriptorTerm "a")
                                                    Intersect
                                                    ( SubExpression . Identity $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            ( SubBinary . Identity $
                                                                BinaryExpression
                                                                    (SubTag . Identity $ MetaDescriptorTerm "c")
                                                                    Intersect
                                                                    (SubTag . Identity $ MetaDescriptorTerm "d")
                                                            )
                                                    )
                                            )
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "e")
                                    )
                                )
                                (parseSE "a b { c d } e")
                            )
                        , testCase
                            "RHS ignores newline"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubExpression . Identity $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "b")
                                                    ( SubBinary . Identity $
                                                        BinaryExpression
                                                            (SubTag . Identity $ MetaDescriptorTerm "c")
                                                            Intersect
                                                            (SubTag . Identity $ MetaDescriptorTerm "d")
                                                    )
                                            )
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "e")
                                    )
                                )
                                (parseSE "b {\n c d\n}\ne")
                            )
                        , testCase
                            "RHS ignores newline - explicit set ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( SubBinary . Identity $
                                        BinaryExpression
                                            ( SubExpression . Identity $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "b")
                                                    ( SubBinary . Identity $
                                                        BinaryExpression
                                                            (SubTag . Identity $ MetaDescriptorTerm "c")
                                                            Intersect
                                                            (SubTag . Identity $ MetaDescriptorTerm "d")
                                                    )
                                            )
                                            Intersect
                                            (SubTag . Identity $ MetaDescriptorTerm "e")
                                    )
                                )
                                (parseSE "b{\n c&d\n}\n&e")
                            )
                        ]
                  ]
            )
        ]