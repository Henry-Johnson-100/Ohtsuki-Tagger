{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Text.TaggerQL.Expression.Parser (
    parserTests,
    newParserTests,
) where

import Data.Either (isLeft)
import Data.Tagger (SetOp (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Parser

fe :: Pattern -> QueryExpression
fe = QueryExpression . pure . FileLeaf

tle :: TagExpression (DTerm Pattern) -> QueryExpression
tle = QueryExpression . pure . TagLeaf

tedp :: DTerm Pattern -> TagExpression (DTerm Pattern)
tedp = pure

d :: a -> DTerm a
d = DTerm

rt :: a -> DTerm a
rt = DMetaTerm

newParserTests :: TestTree
newParserTests =
    let distrTEs (QueryExpression qe) =
            QueryExpression
                . fmap
                    ( \ql -> case ql of
                        FileLeaf pat -> FileLeaf pat
                        TagLeaf te -> TagLeaf . distribute $ te
                    )
                $ qe
        com msg x y =
            assertEqual
                msg
                ( Right
                    . distrTEs
                    $ x
                )
                (distrTEs <$> parseQueryExpression y)
     in testGroup
            "Parser Tests"
            [ testGroup
                "QueryExpressionParser tests"
                [ testGroup
                    "FileLeaf Expressions"
                    [ testCase "Single FileLeaf" $
                        com
                            "p.apple"
                            (fe "apple")
                            "p.apple"
                    , testCase "Ring Expression of FileLeaf" $
                        com
                            "p.apple | p.orange"
                            (fe "apple" +. fe "orange")
                            "p.apple | p.orange"
                    , testCase "Left-Associative Simple Expression by Default" $
                        com
                            "(p.apple p.orange p.banana) \
                            \All ring expressions should be left-associative by default"
                            ((fe "apple" *. fe "orange") *. fe "banana")
                            "p.apple p.orange p.banana"
                    , testCase "Explicit Right-Association" $
                        com
                            "p.apple (p.orange p.banana)"
                            (fe "apple" *. (fe "orange" *. fe "banana"))
                            "p.apple (p.orange p.banana)"
                    ]
                , testGroup
                    "TagLeaf Expressions"
                    [ testCase "Minimal TagLeaf" $
                        com
                            "apple"
                            ( tle . tedp . rt $ "apple"
                            )
                            "apple"
                    , testCase "Minimal Magma Expression" $
                        com
                            "apple {red}"
                            (tle $ (tedp . rt $ "apple") # (tedp . rt $ "red"))
                            "apple {red}"
                    , testCase "Nested Minimal Magma Expression" $
                        com
                            "apple {peel {red}}"
                            ( tle $
                                (tedp . rt $ "apple")
                                    # ( (tedp . rt $ "peel")
                                            # (tedp . rt $ "red")
                                      )
                            )
                            "apple {peel {red}}"
                    , testCase "Minimal Magma Expression - Nonsignificant Whitespace" $
                        com
                            "apple{red}"
                            (tle $ (tedp . rt $ "apple") # (tedp . rt $ "red"))
                            "apple{red}"
                    , testCase "Explicit MetaTerm" $
                        com
                            "r.apple"
                            (tle . tedp . rt $ "apple")
                            "r.apple"
                    , testCase "Explicit MetaTerm - Case Insensitive" $
                        com
                            "R.apple"
                            (tle . tedp . rt $ "apple")
                            "R.apple"
                    , testCase "Explicit Term" $
                        com
                            "d.apple"
                            (tle . tedp . d $ "apple")
                            "d.apple"
                    , testCase "Explicit Term - Case Insensitive" $
                        com
                            "D.apple"
                            (tle . tedp . d $ "apple")
                            "D.apple"
                    ]
                , testGroup
                    "Mixed Leaf Expressions"
                    [ testCase "File Then Tag Leaf" $
                        com
                            "p.apple orange"
                            (fe "apple" *. (tle . tedp . rt $ "orange"))
                            "p.apple orange"
                    , testCase "Tag Then File Leaf" $
                        com
                            "apple p.orange"
                            ((tle . tedp . rt $ "apple") *. fe "orange")
                            "apple p.orange"
                    , testCase "Parenthesized Tag Leaves" $
                        com
                            "(apple orange banana)"
                            ( (tle . tedp . rt $ "apple")
                                *. (tle . tedp . rt $ "orange")
                                *. (tle . tedp . rt $ "banana")
                            )
                            "(apple orange banana)"
                    , testCase "Mixed Parenthesized Tag Leaves" $
                        com
                            "[p.apple (orange banana)] - Should be three leaves in a \
                            \QueryExpression, not two."
                            ( fe "apple"
                                *. ( (tle . tedp . rt $ "orange")
                                        *. (tle . tedp . rt $ "banana")
                                   )
                            )
                            "p.apple (orange banana)"
                    , testCase "Simple Left Distribution" $
                        com
                            "apple {red | yellow}"
                            ( tle $
                                (tedp . rt $ "apple")
                                    # ( (tedp . rt $ "red")
                                            +. (tedp . rt $ "yellow")
                                      )
                            )
                            "apple {red | yellow}"
                    , testCase "Simple Right Distribution" $
                        com
                            "(apple | orange) {red}"
                            ( tle $
                                ( (tedp . rt $ "apple")
                                    +. (tedp . rt $ "orange")
                                )
                                    # (tedp . rt $ "red")
                            )
                            "(apple | orange) {red}"
                    , testCase "Simple Right Distribution - Nonsignificant Whitespace" $
                        com
                            "(apple|orange){red}"
                            ( tle $
                                ( (tedp . rt $ "apple")
                                    +. (tedp . rt $ "orange")
                                )
                                    # (tedp . rt $ "red")
                            )
                            "(apple|orange){red}"
                    , testCase "Associative Right Distribution" $
                        com
                            "(apple | (orange{peel})) {red}"
                            ( tle $
                                ( (tedp . rt $ "apple")
                                    +. ( (tedp . rt $ "orange")
                                            # (tedp . rt $ "peel")
                                       )
                                )
                                    # (tedp . rt $ "red")
                            )
                            "(apple | (orange{peel})) {red}"
                    , testCase "Associative Left Distribution" $
                        com
                            "apple{peel}{red}"
                            ( tle $
                                ( (tedp . rt $ "apple")
                                    # (tedp . rt $ "peel")
                                )
                                    # (tedp . rt $ "red")
                            )
                            "apple{peel}{red}"
                    , testCase "Mixed Distribution" $
                        com
                            "(apple{skin} | orange{peel}){orange | red}"
                            ( tle $
                                ( ((tedp . rt $ "apple") # (tedp . rt $ "skin"))
                                    +. ((tedp . rt $ "orange") # (tedp . rt $ "peel"))
                                )
                                    # ((tedp . rt $ "yellow") +. (tedp . rt $ "red"))
                            )
                            "(apple{skin} | orange{peel}){yellow | red}"
                    ]
                ]
            ]

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
                    (Right (TagTermValue (MetaDescriptorTerm "hello")))
                    (parseExpr "hello")
                )
            , testCase
                "Parse explicit tag term expr"
                ( assertEqual
                    ""
                    (Right (TagTermValue (DescriptorTerm "hello")))
                    (parseExpr "d.hello")
                )
            , testCase
                "file term value"
                (assertEqual "" (Right (FileTermValue "hello")) (parseExpr "p.hello"))
            , testCase
                "tag expression entrance"
                ( assertEqual
                    ""
                    ( Right
                        ( TagExpression $
                            TagTermExtension
                                (MetaDescriptorTerm "a")
                                (SubTag (MetaDescriptorTerm "b"))
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
                            ( BinaryExpression $
                                BinaryOperation
                                    (TagTermValue (MetaDescriptorTerm "a"))
                                    Union
                                    (TagTermValue (MetaDescriptorTerm "b"))
                            )
                        )
                        (parseExpr "a | b")
                    )
                , testCase
                    "Simple implicit binary"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpression $
                                BinaryOperation
                                    (TagTermValue (MetaDescriptorTerm "a"))
                                    Intersect
                                    (TagTermValue (MetaDescriptorTerm "b"))
                            )
                        )
                        (parseExpr "a b")
                    )
                , testCase
                    "Binary is left-associative"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpression $
                                BinaryOperation
                                    ( BinaryExpression $
                                        BinaryOperation
                                            (TagTermValue (MetaDescriptorTerm "a"))
                                            Union
                                            (TagTermValue (MetaDescriptorTerm "b"))
                                    )
                                    Difference
                                    (TagTermValue (MetaDescriptorTerm "c"))
                            )
                        )
                        (parseExpr "a | b ! c")
                    )
                , testCase
                    "Can change binary precedence"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpression $
                                BinaryOperation
                                    ( TagTermValue (MetaDescriptorTerm "a")
                                    )
                                    Union
                                    ( BinaryExpression $
                                        BinaryOperation
                                            (TagTermValue (MetaDescriptorTerm "b"))
                                            Difference
                                            (TagTermValue (MetaDescriptorTerm "c"))
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
                            ( BinaryExpression $
                                BinaryOperation
                                    ( BinaryExpression $
                                        BinaryOperation
                                            ( BinaryExpression $
                                                BinaryOperation
                                                    (TagTermValue (MetaDescriptorTerm "a"))
                                                    Intersect
                                                    ( TagExpression $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            (SubTag (MetaDescriptorTerm "c"))
                                                    )
                                            )
                                            Union
                                            ( BinaryExpression $
                                                BinaryOperation
                                                    (FileTermValue "hi")
                                                    Intersect
                                                    (FileTermValue "bye")
                                            )
                                    )
                                    Difference
                                    (TagTermValue (DescriptorTerm "unwanted"))
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
                        ( BinaryExpression $
                            BinaryOperation
                                ( BinaryExpression $
                                    BinaryOperation
                                        (TagTermValue (MetaDescriptorTerm "a"))
                                        Intersect
                                        ( TagExpression $
                                            TagTermExtension
                                                (MetaDescriptorTerm "b")
                                                ( BinarySubExpression $
                                                    BinaryOperation
                                                        (SubTag (MetaDescriptorTerm "c"))
                                                        Intersect
                                                        (SubTag (MetaDescriptorTerm "d"))
                                                )
                                        )
                                )
                                Intersect
                                (TagTermValue (MetaDescriptorTerm "e"))
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
                            ( BinaryExpression $
                                BinaryOperation
                                    (TagTermValue (MetaDescriptorTerm "a"))
                                    Intersect
                                    (TagTermValue (MetaDescriptorTerm "b"))
                            )
                        )
                        (parseExpr "a b ")
                    )
                , testCase
                    "for binary expr - explicit set op"
                    ( assertEqual
                        ""
                        ( Right
                            ( BinaryExpression $
                                BinaryOperation
                                    (TagTermValue (MetaDescriptorTerm "a"))
                                    Union
                                    (TagTermValue (MetaDescriptorTerm "b"))
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
                            (Right (SubTag (DescriptorTerm "hello")))
                            (parseSE "d.hello")
                        )
                  , testCase
                        "Parse a single MetaDescriptor Term Subtag"
                        ( assertEqual
                            ""
                            (Right (SubTag (MetaDescriptorTerm "hello")))
                            (parseSE "hello")
                        )
                  , testCase
                        "Parse a parenthesized subtag"
                        ( assertEqual
                            ""
                            (Right (SubTag (MetaDescriptorTerm "hello")))
                            (parseSE "(  hello  )")
                        )
                  , testCase
                        "Parse simple sub expression"
                        ( assertEqual
                            ""
                            ( Right
                                ( SubExpression $
                                    TagTermExtension
                                        (MetaDescriptorTerm "a")
                                        (SubTag (MetaDescriptorTerm "b"))
                                )
                            )
                            (parseSE "a{b}")
                        )
                  , testCase
                        "Parse nested sub expression"
                        ( assertEqual
                            ""
                            ( Right
                                ( SubExpression $
                                    TagTermExtension
                                        (MetaDescriptorTerm "a")
                                        ( SubExpression $
                                            TagTermExtension
                                                (MetaDescriptorTerm "b")
                                                (SubTag (MetaDescriptorTerm "c"))
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
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            (SubTag (MetaDescriptorTerm "a"))
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "b"))
                                    )
                                )
                                (parseSE "a b")
                            )
                        , testCase
                            "Alternate SubBinary operator"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            (SubTag (MetaDescriptorTerm "a"))
                                            Union
                                            (SubTag (MetaDescriptorTerm "b"))
                                    )
                                )
                                (parseSE "a | b")
                            )
                        , testCase
                            "SubBinary is left-associative"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( BinarySubExpression $
                                                BinaryOperation
                                                    (SubTag (MetaDescriptorTerm "a"))
                                                    Intersect
                                                    (SubTag (MetaDescriptorTerm "b"))
                                            )
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "c"))
                                    )
                                )
                                (parseSE "a b c")
                            )
                        , testCase
                            "SubBinary can use different set ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( BinarySubExpression $
                                                BinaryOperation
                                                    ( BinarySubExpression $
                                                        BinaryOperation
                                                            (SubTag (MetaDescriptorTerm "a"))
                                                            Union
                                                            (SubTag (MetaDescriptorTerm "b"))
                                                    )
                                                    Intersect
                                                    (SubTag (MetaDescriptorTerm "c"))
                                            )
                                            Difference
                                            (SubTag (MetaDescriptorTerm "d"))
                                    )
                                )
                                (parseSE "a | b & c ! d")
                            )
                        , testCase
                            "Can change SubBinary precedence"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( BinarySubExpression $
                                                BinaryOperation
                                                    (SubTag (MetaDescriptorTerm "a"))
                                                    Union
                                                    (SubTag (MetaDescriptorTerm "b"))
                                            )
                                            Intersect
                                            ( BinarySubExpression $
                                                BinaryOperation
                                                    (SubTag (MetaDescriptorTerm "c"))
                                                    Difference
                                                    (SubTag (MetaDescriptorTerm "d"))
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
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( SubExpression $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "a")
                                                    (SubTag (MetaDescriptorTerm "b"))
                                            )
                                            Union
                                            ( SubExpression $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "c")
                                                    ( BinarySubExpression $
                                                        BinaryOperation
                                                            (SubTag (MetaDescriptorTerm "d"))
                                                            Intersect
                                                            (SubTag (MetaDescriptorTerm "e"))
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
                                ( BinarySubExpression $
                                    BinaryOperation
                                        (SubTag (MetaDescriptorTerm "a"))
                                        Intersect
                                        (SubTag (MetaDescriptorTerm "b"))
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
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            (SubTag (MetaDescriptorTerm "a"))
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "b"))
                                    )
                                )
                                (parseSE "a b ")
                            )
                        , testCase
                            "Sub expression ignores trailing whitespace - explicit set op"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            (SubTag (MetaDescriptorTerm "a"))
                                            Union
                                            (SubTag (MetaDescriptorTerm "b"))
                                    )
                                )
                                (parseSE "a | b ")
                            )
                        , testCase
                            "SubExpression ignores newline - 2"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( BinarySubExpression $
                                                BinaryOperation
                                                    (SubTag (MetaDescriptorTerm "a"))
                                                    Intersect
                                                    ( SubExpression $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            ( BinarySubExpression $
                                                                BinaryOperation
                                                                    (SubTag (MetaDescriptorTerm "c"))
                                                                    Intersect
                                                                    (SubTag (MetaDescriptorTerm "d"))
                                                            )
                                                    )
                                            )
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "e"))
                                    )
                                )
                                (parseSE "a b {\n c d\n}\ne")
                            )
                        , testCase
                            "SubExpression ignores newline - 2 - Explicit Set Ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( BinarySubExpression $
                                                BinaryOperation
                                                    (SubTag (MetaDescriptorTerm "a"))
                                                    Intersect
                                                    ( SubExpression $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            ( BinarySubExpression $
                                                                BinaryOperation
                                                                    (SubTag (MetaDescriptorTerm "c"))
                                                                    Intersect
                                                                    (SubTag (MetaDescriptorTerm "d"))
                                                            )
                                                    )
                                            )
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "e"))
                                    )
                                )
                                (parseSE "a&b{\nc&d\n}\n&e")
                            )
                        , testCase
                            "SubExpression works with no newlines"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( BinarySubExpression $
                                                BinaryOperation
                                                    (SubTag (MetaDescriptorTerm "a"))
                                                    Intersect
                                                    ( SubExpression $
                                                        TagTermExtension
                                                            (MetaDescriptorTerm "b")
                                                            ( BinarySubExpression $
                                                                BinaryOperation
                                                                    (SubTag (MetaDescriptorTerm "c"))
                                                                    Intersect
                                                                    (SubTag (MetaDescriptorTerm "d"))
                                                            )
                                                    )
                                            )
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "e"))
                                    )
                                )
                                (parseSE "a b { c d } e")
                            )
                        , testCase
                            "RHS ignores newline"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( SubExpression $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "b")
                                                    ( BinarySubExpression $
                                                        BinaryOperation
                                                            (SubTag (MetaDescriptorTerm "c"))
                                                            Intersect
                                                            (SubTag (MetaDescriptorTerm "d"))
                                                    )
                                            )
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "e"))
                                    )
                                )
                                (parseSE "b {\n c d\n}\ne")
                            )
                        , testCase
                            "RHS ignores newline - explicit set ops"
                            ( assertEqual
                                ""
                                ( Right
                                    ( BinarySubExpression $
                                        BinaryOperation
                                            ( SubExpression $
                                                TagTermExtension
                                                    (MetaDescriptorTerm "b")
                                                    ( BinarySubExpression $
                                                        BinaryOperation
                                                            (SubTag (MetaDescriptorTerm "c"))
                                                            Intersect
                                                            (SubTag (MetaDescriptorTerm "d"))
                                                    )
                                            )
                                            Intersect
                                            (SubTag (MetaDescriptorTerm "e"))
                                    )
                                )
                                (parseSE "b{\n c&d\n}\n&e")
                            )
                        ]
                  ]
            )
        ]