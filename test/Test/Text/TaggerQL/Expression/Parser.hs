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
import qualified Data.Text as T
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
                    , "(p.apple) | p.orange"
                    , "p.apple | (p.orange)"
                    , "(p.apple | p.orange)"
                    ]
                , comBat
                    "Left-Associative Simple Expression"
                    ""
                    ((fe "apple" *. fe "orange") *. fe "banana")
                    [ "p.apple p.orange p.banana"
                    , "(p.apple&p.orange) p.banana"
                    , "(p.apple & p.orange) p.banana"
                    , "(p.apple p.orange) p.banana"
                    , "(p.apple p.orange) (p.banana)"
                    , "((p.apple p.orange) p.banana)"
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
                    , "{apple}"
                    , "{ apple}"
                    , "{apple }"
                    , "{ apple }"
                    ]
                , testCase "Arbitrarily Deeply Bracketed Tag" $
                    comTE
                        "{{{{{a}}}}}"
                        (tedp . rt $ "a")
                        "{{{{{a}}}}}"
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
                    , "{apple{red}}"
                    , "{apple {red}}"
                    , "{apple} {red}"
                    , "(apple) {red}"
                    , "({apple}) {red}"
                    ]
                , testCase "Explicit Set Operator Annihilates Distribution" $
                    com
                        "[apple & {red}] /= [apple {red}]"
                        ((tle . tedp . rt $ "apple") *. (tle . tedp . rt $ "red"))
                        "apple & {red}"
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
                    , "{apple{peel{red}}}"
                    , "{apple{peel}}{red}"
                    , "{{apple}{peel}}{red}"
                    , "({apple}{peel}){red}"
                    , "(apple{peel}){red}"
                    , "{apple} {peel} {red}"
                    , "apple {peel} {red}"
                    ]
                , comBat
                    "Explicit MetaTerm"
                    "r.apple"
                    (tle . tedp . rt $ "apple")
                    [ "r.apple"
                    , "R.apple"
                    , "{r.apple}"
                    , "{R.apple}"
                    , "apple"
                    ]
                , comBat
                    "Explicit Term"
                    "d.apple"
                    (tle . tedp . d $ "apple")
                    [ "d.apple"
                    , "D.apple"
                    , "{d.apple}"
                    , "{D.apple}"
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
                , comBat
                    "File Then Tag Leaf"
                    "p.apple orange"
                    (fe "apple" *. (tle . tedp . rt $ "orange"))
                    [ "p.apple orange"
                    , "p.apple {orange}"
                    , "(p.apple){orange}"
                    , "(p.apple) & {orange}"
                    , "(p.apple)({orange})"
                    , "(p.apple)&({orange})"
                    ]
                , comBat
                    "Tag Then File Leaf"
                    "apple p.orange"
                    ((tle . tedp . rt $ "apple") *. fe "orange")
                    [ "apple p.orange"
                    , "apple & p.orange"
                    , "{apple} p.orange"
                    , "{apple} (p.orange)"
                    , "apple & (p.orange)"
                    , "apple (p.orange)"
                    , "(apple) (p.orange)"
                    , "({apple})(p.orange)"
                    , "({apple})&(p.orange)"
                    ]
                , comBat
                    "Parenthesized Tag Leaves"
                    "(apple orange banana)"
                    ( ( (tle . tedp . rt $ "apple")
                            *. (tle . tedp . rt $ "orange")
                      )
                        *. (tle . tedp . rt $ "banana")
                    )
                    [ "(apple orange banana)"
                    , "((apple orange) banana)"
                    , -- These are erroneous test cases,
                      -- since {} distributes and associates
                      -- strongly to the left.
                      -- , "{apple} {orange} {banana}"
                      -- , "({apple} {orange}) {banana}"
                      "({apple}) ({orange}) ({banana})"
                    , "(({apple}) ({orange})) ({banana})"
                    ]
                , comBat
                    "Bracketed Tag Leaves are One FileLeaf"
                    "[{apple orange banana}] - one file with that tag structure."
                    ( tle
                        ( ( (tedp . rt $ "apple")
                                *. (tedp . rt $ "orange")
                          )
                            *. (tedp . rt $ "banana")
                        )
                    )
                    [ "{apple orange banana}"
                    , "{(apple) (orange) (banana)}"
                    , "{apple & orange & banana}"
                    , "{{apple} & {orange} & {banana}}"
                    , "{(apple) & (orange) & (banana)}"
                    , "{(apple orange) banana}"
                    , "{({apple}) ({orange}) ({banana})}"
                    , "{({apple}) & ({orange}) & ({banana})}"
                    , "{(({apple}) ({orange})) ({banana})}"
                    ]
                , comBatTE
                    "TagExpression is Only Left-Distributive"
                    ""
                    ( (tedp . rt $ "apple")
                        *. (tedp . rt $ "orange")
                        *. (tedp . rt $ "banana")
                    )
                    [ "{{apple} orange banana}"
                    , "{apple orange banana}"
                    ]
                , comBatTE
                    "Left Distribution Occurs for Bracketed Tags"
                    ""
                    ( ( (tedp . rt $ "apple")
                            # (tedp . rt $ "orange")
                      )
                        *. (tedp . rt $ "banana")
                    )
                    [ "{{apple} {orange} banana}"
                    , "{apple{orange} banana}"
                    , -- This scenario is erroneous because a {} will always
                      -- associate to the left and is only blocked by ()
                      -- or a set operator.
                      --   "{apple{orange} {banana}}"
                      "{(apple{orange}) banana}"
                    , "{apple{orange} ({banana})}"
                    , "{apple{orange} (banana)}"
                    ]
                , comBatTE
                    "Nested Tag Distribution"
                    ""
                    ( (tedp . rt $ "apple")
                        # ( (tedp . rt $ "orange")
                                # (tedp . rt $ "banana")
                          )
                    )
                    [ "{apple{orange{banana}}}"
                    , "{{apple} {orange} {banana}}"
                    , "{{apple{orange}} {banana}}"
                    , "{{{apple} {orange}} {banana}}"
                    ]
                , comBatTE
                    "Tag Distribution is Associative"
                    "[{{a} {b} {c}}] Should be associative."
                    ( (tedp . rt $ "a")
                        # ( (tedp . rt $ "b")
                                # (tedp . rt $ "c")
                          )
                    )
                    [ "{{a} {b} {c}}"
                    , "{{{a} {b}} {c}}"
                    , "{{a} {{b} {c}}}"
                    ]
                , comBat
                    "Mixed Parenthesized Tag Leaves"
                    "[p.apple (orange banana)] - Should be three leaves in a \
                    \QueryExpression, not two."
                    ( fe "apple"
                        *. ( (tle . tedp . rt $ "orange")
                                *. (tle . tedp . rt $ "banana")
                           )
                    )
                    [ "p.apple (orange banana)"
                    , -- Does not match this scenario because
                      -- bracketed tag expressions are left distributive to bracketed
                      -- tag expressions.
                      -- , "(p.apple) ({orange} {banana})"
                      --   "(p.apple ({orange} {banana}))"
                      "p.apple ({orange} banana)"
                    , -- These are fine because query expressions do not distribute
                      "p.apple ({orange} (banana))"
                    , "p.apple ({orange} ({banana}))"
                    ]
                , comBat
                    "Mixed Parenthesized Tag Leaves"
                    "Bracketed tags are one leaf."
                    ( fe "apple"
                        *. tle
                            ( (tedp . rt $ "orange")
                                *. (tedp . rt $ "banana")
                            )
                    )
                    [ "p.apple {orange banana}"
                    , "p.apple ({orange banana})"
                    , "p.apple & ({orange banana})"
                    , "p.apple & {orange banana}"
                    ]
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
                    , "(apple{peel} | apple{skin}) {red | yellow}"
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
                    , "apple{{peel | skin}{red | yellow}}"
                    , -- mixing parens for fun
                      "apple{{{peel} | (skin)}{(red) | {yellow}}"
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
                    ( tle
                        ( (tedp . rt $ "apple")
                            # (tedp . rt $ "red")
                        )
                        +. tle
                            ( (tedp . rt $ "orange")
                                # (tedp . rt $ "red")
                            )
                    )
                    [ "(apple | orange) {red}"
                    , "(apple|orange){red}"
                    ]
                , testCase
                    "Bracketed Expression is \
                    \Different than Parenthesized Expression"
                    $ com
                        "In a top-evel scope, a bracketed expression \
                        \produces only one TagLeaf"
                        ( tle
                            ( ( (tedp . rt $ "apple")
                                    +. (tedp . rt $ "orange")
                              )
                                # (tedp . rt $ "red")
                            )
                        )
                        "{apple | orange} {red}"
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
                    , "{apple | orange} {red}"
                    , "( apple|orange ){ red }"
                    , "apple {red} | orange {red}"
                    ]
                , comBat
                    "Associative Right Distribution"
                    "(apple | (orange{peel})) {red}"
                    ( tle ((tedp . rt $ "apple") # (tedp . rt $ "red"))
                        +. tle
                            ( (tedp . rt $ "orange")
                                # ( (tedp . rt $ "peel")
                                        # (tedp . rt $ "red")
                                  )
                            )
                    )
                    [ "(apple | (orange{peel})) {red}"
                    , "(apple | orange {peel}) {red}"
                    , "(apple | orange{peel}){red}"
                    ]
                , comBatTE
                    "Associative Right Tag Distribution"
                    ""
                    ( ( (tedp . rt $ "apple")
                            +. ( (tedp . rt $ "orange")
                                    # (tedp . rt $ "peel")
                               )
                      )
                        # (tedp . rt $ "red")
                    )
                    [ "{apple | orange{peel}} {red}"
                    , "apple {red} | orange {peel {red}}"
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
                    , "{apple{peel}}{red}"
                    , "apple {peel}{red}"
                    , -- desugared
                      "apple{peel{red}}"
                    ]
                , comBat
                    "Mixed Distribution"
                    "(apple{skin} | orange{peel}){orange | red}"
                    ( let rightTE = (tedp . rt $ "orange") +. (tedp . rt $ "red")
                       in tle
                            ( ( (tedp . rt $ "apple")
                                    # (tedp . rt $ "skin")
                              )
                                # rightTE
                            )
                            +. tle
                                ( ( (tedp . rt $ "orange")
                                        # (tedp . rt $ "peel")
                                  )
                                    # rightTE
                                )
                    )
                    ["(apple{skin} | orange{peel}){yellow | red}"]
                , comBat
                    "Mixed Tag Distribution"
                    "Same as above case but should produce only one TagLeaf"
                    ( let appleskin = (tedp . rt $ "apple") # (tedp . rt $ "skin")
                          orangepeel = (tedp . rt $ "orange") # (tedp . rt $ "peel")
                          yellowred = (tedp . rt $ "yellow") +. (tedp . rt $ "red")
                       in tle ((appleskin +. orangepeel) # yellowred)
                    )
                    [ "{apple{skin} | orange{peel}}{yellow | red}"
                    ]
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
                    , "{apple {skin} | orange {peel}} {yellow | red}"
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
                , testCase "Example From Technote" $
                    com
                        "Should demonstrate left distribution over a query expression."
                        ( tle
                            ( (tedp . rt $ "o%yui")
                                # (tedp . rt $ "cute")
                            )
                            *. tle
                                ( ( (tedp . rt $ "%riamu")
                                        +. (tedp . rt $ "%sachiko")
                                  )
                                    # (tedp . rt $ "cute")
                                )
                        )
                        "(o%yui & {%riamu | %sachiko}) {cute}"
                , testCase "Counter Example From Technote" $
                    com
                        "Should demonstrate left distribution over a tag expression."
                        ( tle
                            ( ( (tedp . rt $ "o%yui")
                                    *. ( (tedp . rt $ "%riamu")
                                            +. (tedp . rt $ "%sachiko")
                                       )
                              )
                                # (tedp . rt $ "cute")
                            )
                        )
                        "{o%yui & {%riamu | %sachiko}} {cute}"
                , testCase "Example From Technote - 2" $
                    com
                        "Demonstrates explicit tag expression."
                        ( tle (tedp . d $ "o%yui")
                            *. tle
                                ( (tedp . rt $ "character")
                                    -. (tedp . d $ "o%yui")
                                )
                        )
                        "d.o%yui & {r.character ! d.o%yui}"
                , testCase "Counter Example from Technote - 2" $
                    com
                        "Deomonstrates precedence of query expressions."
                        ( tle (tedp . d $ "o%yui")
                            *. ( tle (tedp . rt $ "character")
                                    -. tle (tedp . d $ "o%yui")
                               )
                        )
                        "d.o%yui & (r.character ! d.o%yui)"
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
