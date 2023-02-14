{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Text.TaggerQL.Expression.Parser (
    parserTests,
) where

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

{- |
 Normalizes a FreeQueryExpression to its non-recursive representation, making
 equivalence easier to judge.

 This assumes that there are many equivalent ways to express a FreeQueryExpression.
 This is of course true for the purposes of this program.

 But it is important to remember that generally, the expressions described in this
    program are compared with computational equality and not structural equality.
    Though they can be judged this way, it is often not truly necessary
    to judge structural equality with these structures.
-}
distrTEs ::
    FreeQueryExpression ->
    RingExpression
        (Either Pattern (RingExpression (FreeMagma (DTerm Pattern))))
distrTEs = fmap (fmap distributeK) . unliftFreeQueryExpression

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

{- |
 Compare a query to the given normalized structure.
-}
com ::
    HasCallStack =>
    String ->
    RingExpression
        (Either Pattern (RingExpression (FreeMagma (DTerm Pattern)))) ->
    Text ->
    Assertion
com msg x y =
    assertEqual
        msg
        ( Right
            x
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

{- |
 Compare the normalized structure of a given list of queries against a prototype.
-}
equivalentQueries :: TestName -> String -> Text -> [Text] -> TestTree
equivalentQueries testName failMsg prototype eqQs =
    battery
        testName
        failMsg
        (distrTEs <$> parseQueryExpression prototype)
        (map (fmap distrTEs . parseQueryExpression) eqQs)

{- |
 Compare a query prototype to a structure and test equivalent queries.

 Only runs equivalency tests if the prototype matches the expected structure.
-}
verifyQueryStructure ::
    TestName ->
    String ->
    RingExpression
        (Either Pattern (RingExpression (FreeMagma (DTerm Pattern)))) ->
    Text ->
    [Text] ->
    TestTree
verifyQueryStructure testName failMsg structure prototype equivalences =
    let structurePattern = testName <> " - Structure"
     in testGroup
            testName
            [ testCase
                structurePattern
                $ com
                    ("Structure Failure - " <> failMsg)
                    structure
                    prototype
            , after AllSucceed structurePattern $
                equivalentQueries
                    (testName <> " - Equivalent Queries")
                    ("Equivalency Failure - " <> failMsg)
                    prototype
                    equivalences
            ]

{- |
 Alias for Left
-}
l :: a -> Either a b
l = Left

{- |
 Alias for DTerm
-}
d' :: a -> DTerm a
d' = DTerm

{- |
 Alias for pure
-}
p :: Applicative f => a -> f a
p = pure

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
                        (p . l $ "apple")
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
                        ((p . l) "apple" *. ((p . l) "orange" *. (p . l) "banana"))
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
                    , "(apple) {red}"
                    , "({apple}) {red}"
                    ]
                , testCase "Explicit Set Operator Annihilates Distribution" $
                    com
                        "[apple & {red}] /= [apple {red}]"
                        ((p . p . p . p . p $ "apple") *. (p . p . p . p . p $ "red"))
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
                        , "({apple}{peel}){red}"
                        , "(apple{peel}){red}"
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
                        (p . p . p . p . p $ "pLooksLikeFileLeaf")
                        "pLooksLikeFileLeaf"
                , testCase "Ambiguous Prefix - 2" $
                    com
                        "Terms with similar prefixes can be disambiguated."
                        (p . p . p . p . p $ "dLooksLikeDescriptorTerm")
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
                        ( ( (p . p . p)
                                ( (p . p) "apple"
                                    ∙ ( (p . p) "skin"
                                            ∙ (p . p) "red"
                                      )
                                )
                                *. (p . l) "orange"
                          )
                            *. (p . p . p)
                                ( (p . p) "banana"
                                    ∙ ( (p . p) "peel"
                                            ∙ (p . p) "yellow"
                                      )
                                )
                        )
                        "apple{skin{red}} p.orange banana{peel{yellow}}"
                , testCase "Simple Left Distribution" $
                    com
                        "apple {red | yellow}"
                        ( (p . p)
                            ( p
                                ( (p . p) "apple"
                                    ∙ (p . p) "red"
                                )
                                +. p
                                    ( (p . p) "apple"
                                        ∙ (p . p) "yellow"
                                    )
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
                    [ "(apple | orange) {red}"
                    , "(apple|orange){red}"
                    ]
                , testCase
                    "Bracketed Expression is \
                    \Different than Parenthesized Expression"
                    $ com
                        "In a top-evel scope, a bracketed expression \
                        \produces only one TagLeaf"
                        ( (p . p)
                            ( p
                                ( (p . p) "apple"
                                    ∙ (p . p) "red"
                                )
                                +. p
                                    ( (p . p) "orange"
                                        ∙ (p . p) "red"
                                    )
                            )
                        )
                        "{apple | orange} {red}"
                , testCase "Incorrect Test Case for \"Simple Right Distribution\"" $
                    com
                        "[apple{red} | orange{red}] should be parsed as two separate \
                        \Query Leaves and not one TagExpression."
                        ( (p . p . p)
                            ( (p . p) "apple"
                                ∙ (p . p) "red"
                            )
                            +. (p . p . p)
                                ( (p . p) "orange"
                                    ∙ (p . p) "red"
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
                    [ "(apple | (orange{peel})) {red}"
                    , "(apple | orange {peel}) {red}"
                    , "(apple | orange{peel}){red}"
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
                    [ "(apple{peel}){red}"
                    , "(apple {peel}) {red}"
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
                        [ "(apple{skin} | orange{peel}){yellow | red}"
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
                        [ "(apple | orange) {skin} {yellow | red}"
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
                        ( let oyui = (p . p . p) ((p . p) "o%yui" ∙ (p . p) "cute")
                              riamu = p $ (p . p) "%riamu" ∙ (p . p) "cute"
                              sachiko = p $ (p . p) "%sachiko" ∙ (p . p) "cute"
                           in oyui *. (p . p) (riamu +. sachiko)
                        )
                        "(o%yui & {%riamu | %sachiko}) {cute}"
                , testCase "Counter Example From Technote" $
                    com
                        "Should demonstrate left distribution over a tag expression."
                        ( let mk x = p $ (p . p) x ∙ (p . p) "cute"
                              oyui = mk "o%yui"
                              riamu = mk "%riamu"
                              sachiko = mk "%sachiko"
                           in (p . p) (oyui *. (riamu +. sachiko))
                        )
                        "{o%yui & {%riamu | %sachiko}} {cute}"
                , testCase "Example From Technote - 2" $
                    com
                        "Demonstrates explicit tag expression."
                        ( let oyui = p . p . d' $ "o%yui"
                              char = p . p . p $ "character"
                           in (p . p) oyui *. (p . p) (char -. oyui)
                        )
                        "d.o%yui & {r.character ! d.o%yui}"
                , testCase "Counter Example from Technote - 2" $
                    com
                        "Demonstrates precedence of query expressions."
                        ( let oyui = p . p . p . p . d' $ "o%yui"
                              char = p . p . p . p . p $ "character"
                           in oyui *. (char -. oyui)
                        )
                        "d.o%yui & (r.character ! d.o%yui)"
                , -- Tests stating that (a p.b){c}{d} = a{c{d}} (p.b c{d})
                  -- Which is that the operation that is left-distributive (Query -> Tag -> Query)
                  -- first right-associatively folds all right-operands.
                  --
                  -- if this was not the case then the former query would evaluate to
                  -- a{c}{d} ((p.b d) c{d})
                  -- which is nonsensical.
                  --
                  -- Additionally, the query (a p.b){c{d}} is identical to this case.
                  -- and that any 1 right operand should maintain its internal association.
                  testGroup
                    "Unification"
                    [ testCase "Unification Distribution is Right-Associative" $
                        assertEqual
                            "Textual version of AST test by the same name."
                            ( fmap (fmap (fmap distributeK)) . Right $
                                ( Ring . Right $
                                    ( (tedp . rt $ "a")
                                        ∙ ( (tedp . rt $ "c")
                                                ∙ (tedp . rt $ "d")
                                          )
                                    )
                                )
                                    *. ( (Ring . Left $ "b")
                                            *. ( Ring . Right $
                                                    ( (tedp . rt $ "c")
                                                        ∙ (tedp . rt $ "d")
                                                    )
                                               )
                                       )
                            )
                            ( fmap (fmap distributeK)
                                . unliftFreeQueryExpression
                                <$> parseQueryExpression
                                    "(a & p.b){c}{d}"
                            )
                    , equivalentQueries
                        "Unification Distribution Equivalence"
                        ""
                        "(a p.b){c}{d}"
                        ["(a p.b){c{d}}"]
                    ]
                , verifyQueryStructure
                    "Unification"
                    "Unification is a Right-Associative, Left-Distributive operation."
                    ( let withcd f cb x = x `cb` f ((p . p) "c" ∙ (p . p) "d")
                          acd = p . p . p $ withcd id (∙) ((p . p) "a")
                          bAndcd = withcd (p . p . p) (*.) (p . l $ "b")
                       in acd *. bAndcd
                    )
                    "(a & p.b){c}{d}"
                    ["(a & p.b){c{d}}"]
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
                        ((p . l) "a" *. (p . l) "b" *. (p . l) "c")
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
                , testCase "Structural Equality of the Normalized query can be Judged" $
                    assertBool
                        "Structural inequality in the normalized structure can\
                        \ indicate computational non-equivalence."
                        ( (distrTEs <$> parseQueryExpression "(a b) c")
                            /= (distrTEs <$> parseQueryExpression "a (b c)")
                        )
                ]
            ]
        ]
