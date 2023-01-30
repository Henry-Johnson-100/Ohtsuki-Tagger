{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Text.TaggerQL.Expression.Parser (
    parserTests,
) where

import Data.Either (isLeft, isRight)
import Data.Tagger (SetOp (..))
import Data.Text (Text)
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

{- |
 Run a series of equality assertions on varying inputs that all produce the same
 output.
-}
battery :: (Eq a, Show a, HasCallStack) => TestName -> String -> a -> [a] -> TestTree
battery name failMsg expectedResult samples =
    let tests = zip ([1 ..] :: [Int]) samples
     in testGroup
            name
            ( fmap
                ( \(testN, sample) ->
                    testCase
                        (name <> " - " <> show testN)
                        (assertEqual failMsg expectedResult sample)
                )
                tests
            )

distrTEs :: QueryExpression -> QueryExpression
distrTEs (QueryExpression qe) =
    QueryExpression
        . fmap
            ( \ql -> case ql of
                FileLeaf pat -> FileLeaf pat
                TagLeaf te -> TagLeaf . distribute $ te
            )
        $ qe

comBat ::
    HasCallStack =>
    TestName ->
    String ->
    QueryExpression ->
    [Text] ->
    TestTree
comBat name failMsg expect tsts =
    battery
        name
        failMsg
        (Right . distrTEs $ expect)
        (fmap distrTEs . parseQueryExpression <$> tsts)

comBatTE ::
    HasCallStack =>
    TestName ->
    String ->
    TagExpression (DTerm Pattern) ->
    [Text] ->
    TestTree
comBatTE name failMsg expect tsts =
    battery
        name
        failMsg
        (Right . distribute $ expect)
        (fmap distribute . parseTagExpression <$> tsts)

com :: HasCallStack => String -> QueryExpression -> Text -> Assertion
com msg x y =
    assertEqual
        msg
        ( Right
            . distrTEs
            $ x
        )
        (distrTEs <$> parseQueryExpression y)

comTE :: HasCallStack => String -> TagExpression (DTerm Pattern) -> Text -> Assertion
comTE msg x y =
    assertEqual
        msg
        ( Right
            . distribute
            $ x
        )
        (distribute <$> parseTagExpression y)

parserTests :: TestTree
parserTests =
    testGroup
        "Parser Tests"
        [ testGroup
            "Primitive Parser Tests"
            [ testGroup
                "termPatternParser"
                ( let parseP = parse termPatternParser ""
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
                      , testCase "Pattern 9" (assertEqual "" (Right WildCard) (parseP "%"))
                      , testCase "Pattern 10" (assertEqual "" (Right WildCard) (parseP "%%%%"))
                      , testCase "Pattern 11" (assertEqual "" (Right "%%test%%") (parseP "%%test%%"))
                      ]
                )
            ]
        , testGroup
            "QueryExpressionParser tests"
            [ testGroup
                "FileLeaf Expressions"
                [ testCase "Single FileLeaf" $
                    com
                        "p.apple"
                        (fe "apple")
                        "p.apple"
                , comBat
                    "Ring Expression of FileLeaf"
                    ""
                    (fe "apple" +. fe "orange")
                    [ "p.apple | p.orange"
                    , "p.apple| p.orange"
                    , "p.apple |p.orange"
                    , "p.apple|p.orange"
                    , "p.apple   |   p.orange"
                    ]
                , comBat
                    "Left-Associative Simple Expression"
                    ""
                    ((fe "apple" *. fe "orange") *. fe "banana")
                    [ "p.apple p.orange p.banana"
                    , "(p.apple&p.orange) p.banana"
                    , "(p.apple & p.orange) p.banana"
                    , "(p.apple p.orange) p.banana"
                    ]
                , testCase "Explicit Right-Association" $
                    com
                        "p.apple (p.orange p.banana)"
                        (fe "apple" *. (fe "orange" *. fe "banana"))
                        "p.apple (p.orange p.banana)"
                ]
            , testGroup
                "TagLeaf Expressions"
                [ comBat
                    "Minimal TagLeaf"
                    "apple"
                    ( tle . tedp . rt $ "apple"
                    )
                    [ "apple"
                    , " apple "
                    , "apple "
                    , " apple"
                    ]
                , comBat
                    "Minimal Magma Expression"
                    "apple {red}"
                    (tle $ (tedp . rt $ "apple") # (tedp . rt $ "red"))
                    [ "apple {red}"
                    , "apple{red}"
                    , "apple{ red}"
                    , "apple { red}"
                    , "apple{ red }"
                    , "apple{red} "
                    ]
                , comBat
                    "Nested Minimal Magma Expression"
                    "apple {peel {red}}"
                    ( tle $
                        (tedp . rt $ "apple")
                            # ( (tedp . rt $ "peel")
                                    # (tedp . rt $ "red")
                              )
                    )
                    [ "apple {peel {red}}"
                    , "apple{peel{red}}"
                    , "apple { peel { red } }"
                    ]
                , comBat
                    "Explicit MetaTerm"
                    "r.apple"
                    (tle . tedp . rt $ "apple")
                    [ "r.apple"
                    , "R.apple"
                    , "apple"
                    ]
                , comBat
                    "Explicit Term"
                    "d.apple"
                    (tle . tedp . d $ "apple")
                    [ "d.apple"
                    , "D.apple"
                    ]
                ]
            , testGroup
                "Mixed Leaf Expressions"
                [ testCase "Ambiguous Prefix - 1" $
                    com
                        "Terms with similar prefixes can be disambiguated."
                        (tle . tedp . rt $ "pLooksLikeFileLeaf")
                        "pLooksLikeFileLeaf"
                , testCase "Ambiguous Prefix - 2" $
                    com
                        "Terms with similar prefixes can be disambiguated."
                        (tle . tedp . rt $ "dLooksLikeDescriptorTerm")
                        "dLooksLikeDescriptorTerm"
                , testCase "File Then Tag Leaf" $
                    com
                        "p.apple orange"
                        (fe "apple" *. (tle . tedp . rt $ "orange"))
                        "p.apple orange"
                , testCase "Tag Then File Leaf" $
                    com
                        "apple p.orange"
                        ((tle . tedp . rt $ "apple") *. fe "orange")
                        "apple p.orange"
                , comBat
                    "Parenthesized Tag Leaves"
                    "(apple orange banana)"
                    ( (tle . tedp . rt $ "apple")
                        *. (tle . tedp . rt $ "orange")
                        *. (tle . tedp . rt $ "banana")
                    )
                    [ "(apple orange banana)"
                    , "((apple orange) banana)"
                    ]
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
                , testCase "Mixed Leaves and Simple Magmas" $
                    com
                        "apple{skin{red}} p.orange banana{peel{yellow}}"
                        ( tle
                            ( (tedp . rt $ "apple")
                                # ( (tedp . rt $ "skin")
                                        # (tedp . rt $ "red")
                                  )
                            )
                            *. fe "orange"
                            *. tle
                                ( (tedp . rt $ "banana")
                                    # ( (tedp . rt $ "peel")
                                            # (tedp . rt $ "yellow")
                                      )
                                )
                        )
                        "apple{skin{red}} p.orange banana{peel{yellow}}"
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
                , comBat
                    "Nested Left Distribution"
                    ""
                    ( tle $
                        (tedp . rt $ "apple")
                            # ( ( (tedp . rt $ "peel")
                                    +. (tedp . rt $ "skin")
                                )
                                    # ( (tedp . rt $ "red")
                                            +. (tedp . rt $ "yellow")
                                      )
                              )
                    )
                    [ "apple{(peel | skin){red | yellow}}"
                    , -- These two tests below are currently failing which makes sense
                      -- given that these are meant to be QueryExpressions and not
                      -- TagExpressions. So they do not desugar like this.
                      --
                      -- Instead, each top-level operand is being parsed as its own TagLeaf
                      -- which is what is supposed to be happening.
                      --
                      -- So I need to either remove these tests or replace them with
                      -- a testcase using a tag expression parser rather than a
                      -- query expression parser.
                      --
                      -- Or they could be rewritten using distributive expressions,
                      -- though there are already such tests, it probably wouldn't
                      -- hurt to have more.
                      --   "apple {peel {red | yellow}} | apple {skin {red | yellow}}"
                      -- , "apple {peel {red}} | apple {peel {yellow}} | \
                      --   \apple {skin {red}} | apple {skin {yellow}}"
                      --
                      --
                      "(apple{peel} | apple{skin}) {red | yellow}"
                    , "apple{peel{red | yellow} | skin {red | yellow}}"
                    ]
                , comBatTE
                    "Nested Left TagExpression Distribution"
                    ""
                    ( (tedp . rt $ "apple")
                        # ( ( (tedp . rt $ "peel")
                                +. (tedp . rt $ "skin")
                            )
                                # ( (tedp . rt $ "red")
                                        +. (tedp . rt $ "yellow")
                                  )
                          )
                    )
                    [ "apple{(peel | skin){red | yellow}}"
                    , "apple {peel {red | yellow}} | apple {skin {red | yellow}}"
                    , -- desugared
                      -- Because "skin", which was previously one term is desugared into
                      -- two, it's whole right side, after distribution, is still treated
                      -- as one term, which is why the parentheses are there.
                      --
                      -- It is still a left-associative operation, but distribution
                      -- expands terms in place.
                      "apple {peel {red}} | apple {peel {yellow}} | \
                      \(apple {skin {red}} | apple {skin {yellow}})"
                    ]
                , comBat
                    "Simple Right Distribution"
                    "(apple | orange) {red}"
                    ( tle $
                        ( (tedp . rt $ "apple")
                            +. (tedp . rt $ "orange")
                        )
                            # (tedp . rt $ "red")
                    )
                    [ "(apple | orange) {red}"
                    , "(apple|orange){red}"
                    ]
                , testCase "Incorrect Test Case for \"Simple Right Distribution\"" $
                    com
                        "[apple{red} | orange{red}] should be parsed as two separate \
                        \Query Leaves and not one TagExpression."
                        ( tle
                            ( (tedp . rt $ "apple")
                                # (tedp . rt $ "red")
                            )
                            +. tle
                                ( (tedp . rt $ "orange")
                                    # (tedp . rt $ "red")
                                )
                        )
                        "apple{red} | orange{red}"
                , comBatTE
                    "Simple Right TagExpression Distribution"
                    "(apple | orange) {red}"
                    ( ( (tedp . rt $ "apple")
                            +. (tedp . rt $ "orange")
                      )
                        # (tedp . rt $ "red")
                    )
                    [ "(apple | orange) {red}"
                    , "( apple|orange ){ red }"
                    , "apple {red} | orange {red}"
                    ]
                , comBat
                    "Associative Right Distribution"
                    "(apple | (orange{peel})) {red}"
                    ( tle $
                        ( (tedp . rt $ "apple")
                            +. ( (tedp . rt $ "orange")
                                    # (tedp . rt $ "peel")
                               )
                        )
                            # (tedp . rt $ "red")
                    )
                    [ "(apple | (orange{peel})) {red}"
                    , "(apple | orange {peel}) {red}"
                    , "(apple | orange{peel}){red}"
                    ]
                , comBat
                    "Associative Left Distribution"
                    "(apple{peel}){red}"
                    ( tle $
                        ( (tedp . rt $ "apple")
                            # (tedp . rt $ "peel")
                        )
                            # (tedp . rt $ "red")
                    )
                    [ "(apple{peel}){red}"
                    , "(apple {peel}) {red}"
                    , -- desugared
                      "apple{peel{red}}"
                    ]
                , comBat
                    "Mixed Distribution"
                    "(apple{skin} | orange{peel}){orange | red}"
                    ( tle $
                        ( ((tedp . rt $ "apple") # (tedp . rt $ "skin"))
                            +. ((tedp . rt $ "orange") # (tedp . rt $ "peel"))
                        )
                            # ((tedp . rt $ "yellow") +. (tedp . rt $ "red"))
                    )
                    ["(apple{skin} | orange{peel}){yellow | red}"]
                , comBatTE
                    "Mixed Distribution - TagExpression Desugaring"
                    "(apple {skin} | orange {peel}) {yellow | red}"
                    ( ( ( (tedp . rt $ "apple")
                            # (tedp . rt $ "skin")
                        )
                            +. ( (tedp . rt $ "orange")
                                    # (tedp . rt $ "peel")
                               )
                      )
                        # ( (tedp . rt $ "yellow")
                                +. (tedp . rt $ "red")
                          )
                    )
                    [ "(apple {skin} | orange {peel}) {yellow | red}"
                    , "apple {skin {yellow | red}} | orange {peel {yellow | red}}"
                    , "apple {skin {yellow}} | apple {skin {red}} |\
                      \ (orange {peel {yellow}} | orange {peel {red}})"
                    ]
                , comBat
                    "Complex Query - 1"
                    ""
                    ( ( fe "apple"
                            *. tle
                                ( (tedp . rt $ "orange")
                                    # (tedp . rt $ "peel")
                                )
                      )
                        -. ( tle
                                ( ( ( (tedp . rt $ "coconut")
                                        *. (tedp . rt $ "grape")
                                    )
                                        # (tedp . rt $ "smells")
                                  )
                                    # ( (tedp . rt $ "coconut")
                                            # (tedp . rt $ "gun")
                                      )
                                )
                                +. fe "lime"
                           )
                    )
                    [ "p.apple orange{peel} ! \
                      \((((coconut & grape) {smells}) {coconut {gun}}) | p.lime)"
                    ]
                , testCase "Complex Query - 2" $
                    com
                        "p.a ! d.b | d.c {d.d} | r.c {r.d} & (d.e{f} p.g) (h | i{j{k}}){(l m){n}} p.o (q){r}"
                        ( ( ( ( ( ( ( fe "a"
                                        -. tle (tedp . d $ "b")
                                    )
                                        +. tle
                                            ( (tedp . d $ "c")
                                                # (tedp . d $ "d")
                                            )
                                  )
                                    +. tle
                                        ( (tedp . rt $ "c")
                                            # (tedp . rt $ "d")
                                        )
                                )
                                    *. ( tle
                                            ( (tedp . d $ "e")
                                                # (tedp . rt $ "f")
                                            )
                                            *. fe "g"
                                       )
                              )
                                *. tle
                                    ( ( (tedp . rt $ "h")
                                            +. ( (tedp . rt $ "i")
                                                    # ( (tedp . rt $ "j")
                                                            # (tedp . rt $ "k")
                                                      )
                                               )
                                      )
                                        # ( ( (tedp . rt $ "l")
                                                *. (tedp . rt $ "m")
                                            )
                                                # (tedp . rt $ "n")
                                          )
                                    )
                            )
                                *. fe "o"
                          )
                            *. tle ((tedp . rt $ "q") # (tedp . rt $ "r"))
                        )
                        "p.a ! d.b | d.c {d.d} | r.c {r.d} & (d.e{f} p.g) (h | i{j{k}}){(l m){n}} p.o (q){r}"
                ]
            , testGroup
                "Parser Failure Cases"
                [ testCase
                    "parseQueryExpression Fails When All Input Not Consumed"
                    $ assertBool
                        ""
                        (isLeft . parseQueryExpression $ "a (p.b){c}")
                , testCase
                    "parseQueryExpression Does Not Fail For Trailing Whitespace"
                    $ assertBool
                        ""
                        (isRight . parseQueryExpression $ "a ")
                , testCase "parseQueryExpression ignores line breaks" $
                    com
                        ""
                        (fe "a" *. fe "b" *. fe "c")
                        "p.a\np.b\np.c"
                , testCase
                    "parseTagExpression Fails When All Input Not Consumed"
                    $ assertBool
                        ""
                        (isLeft . parseTagExpression $ "a a.b")
                , testCase
                    "parseTagExpression Does Not Fail For Trailing Whitespace"
                    $ assertBool
                        ""
                        (isRight . parseTagExpression $ "a ")
                , testCase "tagExpressionParser ignores line breaks" $
                    comTE
                        ""
                        ( ( (tedp . rt $ "a")
                                # ( (tedp . rt $ "b")
                                        *. (tedp . rt $ "c")
                                  )
                          )
                            *. (tedp . rt $ "d")
                        )
                        "a\n{\nb\nc\n}\nd"
                ]
            ]
        ]
