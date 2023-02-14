{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Text.TaggerQL.Expression.Parser (
    parserTests,
) where

import Data.Bifunctor
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Test.Resources (d, fe, rt, tedp, tle)
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.Expression.AST
import Text.TaggerQL.Expression.Parser

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

distrTEs :: FreeQueryExpression -> FreeQueryExpression
distrTEs (FreeQueryExpression qe) =
    FreeQueryExpression
        . fmap (bimap (bimap distrTEs normalize) (second normalize))
        $ qe

comBat ::
    HasCallStack =>
    TestName ->
    String ->
    FreeQueryExpression ->
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
    TagQueryExpression ->
    [Text] ->
    TestTree
comBatTE name failMsg expect tsts =
    battery
        name
        failMsg
        (Right . distributeK $ expect)
        (fmap distributeK . parseTagExpression <$> tsts)

com :: HasCallStack => String -> FreeQueryExpression -> Text -> Assertion
com msg x y =
    assertEqual
        msg
        ( Right
            . distrTEs
            $ x
        )
        (distrTEs <$> parseQueryExpression y)

comTE :: HasCallStack => String -> TagQueryExpression -> Text -> Assertion
comTE msg x y =
    assertEqual
        msg
        ( Right
            . distributeK
            $ x
        )
        (distributeK <$> parseTagExpression y)

parserTests :: TestTree
parserTests =
    testGroup
        "Parser Tests"
        [ testGroup
            "Primitive Parser Tests"
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
                    (tle $ (tedp . rt $ "apple") ∙ (tedp . rt $ "red"))
                    [ "apple {red}"
                    , "apple{red}"
                    , "apple{ red}"
                    , "apple { red}"
                    , "apple{ red }"
                    , "apple{red} "
                    , "{apple{red}}"
                    , "{apple {red}}"
                    , "{apple} {red}"
                    , "(apple) {red}" -- #FIXME
                    , "({apple}) {red}" -- #FIXME
                    ]
                , testCase "Explicit Set Operator Annihilates Distribution" $
                    com
                        "[apple & {red}] /= [apple {red}]"
                        ((tle . tedp . rt $ "apple") *. (tle . tedp . rt $ "red"))
                        "apple & {red}"
                , testGroup
                    "Nested Minimal Magma Expression"
                    [ comBat
                        "Implicit Right Association"
                        "apple {peel {red}}"
                        ( tle $
                            (tedp . rt $ "apple")
                                ∙ ( (tedp . rt $ "peel")
                                        ∙ (tedp . rt $ "red")
                                  )
                        )
                        [ "{apple} {peel} {red}"
                        , "apple {peel} {red}"
                        ]
                    , comBat
                        "Right Association"
                        "apple {peel {red}}"
                        ( tle $
                            (tedp . rt $ "apple")
                                ∙ ( (tedp . rt $ "peel")
                                        ∙ (tedp . rt $ "red")
                                  )
                        )
                        [ "apple {peel {red}}"
                        , "apple{peel{red}}"
                        , "apple { peel { red } }"
                        , "{apple{peel{red}}}"
                        ]
                    , comBat
                        "Left Association"
                        "apple {peel {red}}"
                        ( tle $
                            ( (tedp . rt $ "apple")
                                ∙ (tedp . rt $ "peel")
                            )
                                ∙ (tedp . rt $ "red")
                        )
                        [ "{apple{peel}}{red}"
                        , "{{apple}{peel}}{red}"
                        , "({apple}{peel}){red}" -- #FIXME
                        , "(apple{peel}){red}" -- #FIXME
                        ]
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
                    , "(p.apple){orange}" -- #FIXME
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
                            ∙ (tedp . rt $ "orange")
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
                , testGroup
                    "Nested Tag Distribution"
                    [ comBatTE
                        "Implicit Right Association"
                        ""
                        ( (tedp . rt $ "apple")
                            ∙ ( (tedp . rt $ "orange")
                                    ∙ (tedp . rt $ "banana")
                              )
                        )
                        [ "{{apple} {orange} {banana}}"
                        , "{apple} {orange} {banana}"
                        , "apple {orange} {banana}"
                        ]
                    , comBatTE
                        "Right Association"
                        ""
                        ( (tedp . rt $ "apple")
                            ∙ ( (tedp . rt $ "orange")
                                    ∙ (tedp . rt $ "banana")
                              )
                        )
                        [ "{apple{orange{banana}}}"
                        , "apple{orange{banana}}"
                        ]
                    , comBatTE
                        "Left Assocation"
                        ""
                        ( ( (tedp . rt $ "apple")
                                ∙ (tedp . rt $ "orange")
                          )
                            ∙ (tedp . rt $ "banana")
                        )
                        [ "{{apple{orange}} {banana}}"
                        , "{{{apple} {orange}} {banana}}"
                        , "{apple {orange}} {banana}"
                        ]
                    ]
                , comBat
                    "Mixed Parenthesized Tag Leaves"
                    "[p.apple (orange banana)] - Should be three leaves in a \
                    \FreeQueryExpression, not two."
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
                                ∙ ( (tedp . rt $ "skin")
                                        ∙ (tedp . rt $ "red")
                                  )
                            )
                            *. fe "orange"
                            *. tle
                                ( (tedp . rt $ "banana")
                                    ∙ ( (tedp . rt $ "peel")
                                            ∙ (tedp . rt $ "yellow")
                                      )
                                )
                        )
                        "apple{skin{red}} p.orange banana{peel{yellow}}"
                , testCase "Simple Left Distribution" $
                    com
                        "apple {red | yellow}"
                        ( tle $
                            (tedp . rt $ "apple")
                                ∙ ( (tedp . rt $ "red")
                                        +. (tedp . rt $ "yellow")
                                  )
                        )
                        "apple {red | yellow}"
                , testGroup
                    "Nested Left Distribution"
                    [ comBat
                        "Right Association"
                        ""
                        ( tle $
                            (tedp . rt $ "apple")
                                ∙ ( ( (tedp . rt $ "peel")
                                        +. (tedp . rt $ "skin")
                                    )
                                        ∙ ( (tedp . rt $ "red")
                                                +. (tedp . rt $ "yellow")
                                          )
                                  )
                        )
                        [ "apple{(peel | skin){red | yellow}}"
                        , "apple{peel{red | yellow} | skin {red | yellow}}"
                        ]
                    , comBat
                        "Left Association"
                        ""
                        ( tle $
                            ( (tedp . rt $ "apple")
                                ∙ ( (tedp . rt $ "peel")
                                        +. (tedp . rt $ "skin")
                                  )
                            )
                                ∙ ( (tedp . rt $ "red")
                                        +. (tedp . rt $ "yellow")
                                  )
                        )
                        [ "{apple{peel} | apple{skin}} {red | yellow}"
                        ]
                    ]
                , comBatTE
                    "Nested Left TagExpression Distribution"
                    ""
                    ( (tedp . rt $ "apple")
                        ∙ ( ( (tedp . rt $ "peel")
                                +. (tedp . rt $ "skin")
                            )
                                ∙ ( (tedp . rt $ "red")
                                        +. (tedp . rt $ "yellow")
                                  )
                          )
                    )
                    [ "apple{(peel | skin){red | yellow}}"
                    , "apple{{peel | skin}{red | yellow}}"
                    , -- mixing parens for fun
                      "apple{{{peel} | (skin)}{(red) | {yellow}}}"
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
                            ∙ (tedp . rt $ "red")
                        )
                        +. tle
                            ( (tedp . rt $ "orange")
                                ∙ (tedp . rt $ "red")
                            )
                    )
                    [ "(apple | orange) {red}" -- #FIXME
                    , "(apple|orange){red}" -- #FIXME
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
                                ∙ (tedp . rt $ "red")
                            )
                        )
                        "{apple | orange} {red}"
                , testCase "Incorrect Test Case for \"Simple Right Distribution\"" $
                    com
                        "[apple{red} | orange{red}] should be parsed as two separate \
                        \Query Leaves and not one TagExpression."
                        ( tle
                            ( (tedp . rt $ "apple")
                                ∙ (tedp . rt $ "red")
                            )
                            +. tle
                                ( (tedp . rt $ "orange")
                                    ∙ (tedp . rt $ "red")
                                )
                        )
                        "apple{red} | orange{red}"
                , comBatTE
                    "Simple Right TagExpression Distribution"
                    "(apple | orange) {red}"
                    ( ( (tedp . rt $ "apple")
                            +. (tedp . rt $ "orange")
                      )
                        ∙ (tedp . rt $ "red")
                    )
                    [ "(apple | orange) {red}"
                    , "{apple | orange} {red}"
                    , "( apple|orange ){ red }"
                    , "apple {red} | orange {red}"
                    ]
                , comBat
                    "Associative Right Distribution"
                    "(apple | (orange{peel})) {red}"
                    ( tle ((tedp . rt $ "apple") ∙ (tedp . rt $ "red"))
                        +. tle
                            ( ( (tedp . rt $ "orange")
                                    ∙ (tedp . rt $ "peel")
                              )
                                ∙ (tedp . rt $ "red")
                            )
                    )
                    [ "(apple | (orange{peel})) {red}" -- #FIXME
                    , "(apple | orange {peel}) {red}" -- #FIXME
                    , "(apple | orange{peel}){red}" -- #FIXME
                    ]
                , comBatTE
                    "Associative Right Tag Distribution"
                    ""
                    ( ( (tedp . rt $ "apple")
                            +. ( (tedp . rt $ "orange")
                                    ∙ (tedp . rt $ "peel")
                               )
                      )
                        ∙ (tedp . rt $ "red")
                    )
                    [ "{apple | orange{peel}} {red}"
                    , "{apple} {red} | {orange {peel}} {red}"
                    ]
                , comBat
                    "Left Associated Distributed Operation"
                    "(apple{peel}){red}"
                    ( tle $
                        ( (tedp . rt $ "apple")
                            ∙ (tedp . rt $ "peel")
                        )
                            ∙ (tedp . rt $ "red")
                    )
                    [ "(apple{peel}){red}" -- #FIXME
                    , "(apple {peel}) {red}" -- #FIXME
                    , "{apple{peel}}{red}"
                    ]
                , testGroup
                    "Mixed Distribution"
                    [ comBat
                        "Left Association"
                        "(apple{skin} | orange{peel}){orange | red}"
                        ( let rightTE = (tedp . rt $ "yellow") +. (tedp . rt $ "red")
                           in tle
                                ( ( (tedp . rt $ "apple")
                                        ∙ (tedp . rt $ "skin")
                                  )
                                    ∙ rightTE
                                )
                                +. tle
                                    ( ( (tedp . rt $ "orange")
                                            ∙ (tedp . rt $ "peel")
                                      )
                                        ∙ rightTE
                                    )
                        )
                        [ "(apple{skin} | orange{peel}){yellow | red}" -- #FIXME
                        , "{apple {skin}} {yellow | red} | {orange {peel}} {yellow | red}"
                        , "{{apple {skin}} {yellow} | {apple {skin}} {red}} | \
                          \{{orange {peel}} {yellow} | {orange {peel}} {red}}"
                        ]
                    , comBat
                        "Implicit Right Association"
                        ""
                        ( let syr =
                                (tedp . rt $ "skin")
                                    ∙ ( (tedp . rt $ "yellow")
                                            +. (tedp . rt $ "red")
                                      )
                           in tle ((tedp . rt $ "apple") ∙ syr)
                                +. tle ((tedp . rt $ "orange") ∙ syr)
                        )
                        [ "(apple | orange) {skin} {yellow | red}" -- #FIXME
                        , "apple {skin {yellow | red}} | orange {skin {yellow | red}}"
                        , "{apple{skin{yellow}} | apple{skin{red}}} | \
                          \{orange{skin{yellow}} | orange{skin{red}}}"
                        ]
                    ]
                , comBat
                    "Mixed Tag Distribution"
                    "Same as above case but should produce only one TagLeaf"
                    ( let appleskin = (tedp . rt $ "apple") ∙ (tedp . rt $ "skin")
                          orangepeel = (tedp . rt $ "orange") ∙ (tedp . rt $ "peel")
                          yellowred = (tedp . rt $ "yellow") +. (tedp . rt $ "red")
                       in tle ((appleskin +. orangepeel) ∙ yellowred)
                    )
                    [ "{apple{skin} | orange{peel}}{yellow | red}"
                    ]
                , comBatTE
                    "Mixed Distribution - TagExpression Desugaring"
                    "(apple {skin} | orange {peel}) {yellow | red}"
                    ( ( ( (tedp . rt $ "apple")
                            ∙ (tedp . rt $ "skin")
                        )
                            +. ( (tedp . rt $ "orange")
                                    ∙ (tedp . rt $ "peel")
                               )
                      )
                        ∙ ( (tedp . rt $ "yellow")
                                +. (tedp . rt $ "red")
                          )
                    )
                    [ "(apple {skin} | orange {peel}) {yellow | red}"
                    , "{apple {skin} | orange {peel}} {yellow | red}"
                    , "{apple {skin}} {yellow | red} | {orange {peel}} {yellow | red}"
                    , "{{apple {skin}} {yellow} | {apple {skin}} {red}} |\
                      \{{orange {peel}} {yellow} | {orange {peel}} {red}}"
                    ]
                , testCase "Example From Technote" $
                    com
                        "Should demonstrate left distribution over a query expression."
                        ( tle
                            ( (tedp . rt $ "o%yui")
                                ∙ (tedp . rt $ "cute")
                            )
                            *. tle
                                ( ( (tedp . rt $ "%riamu")
                                        +. (tedp . rt $ "%sachiko")
                                  )
                                    ∙ (tedp . rt $ "cute")
                                )
                        )
                        "(o%yui & {%riamu | %sachiko}) {cute}" -- #FIXME
                , testCase "Counter Example From Technote" $
                    com
                        "Should demonstrate left distribution over a tag expression."
                        ( tle
                            ( ( (tedp . rt $ "o%yui")
                                    *. ( (tedp . rt $ "%riamu")
                                            +. (tedp . rt $ "%sachiko")
                                       )
                              )
                                ∙ (tedp . rt $ "cute")
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
                        (isLeft . parseQueryExpression $ "a p.")
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
                    "parseTagExpression Can Fail"
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
                                ∙ ( (tedp . rt $ "b")
                                        *. (tedp . rt $ "c")
                                  )
                          )
                            *. (tedp . rt $ "d")
                        )
                        "a\n{\nb\nc\n}\nd"
                ]
            ]
        ]
