{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

import Data.List.NonEmpty
import Data.Tagger
import qualified Data.Text as T
import Test.Database.Tagger.Main
import Test.Resources
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.AST
import Text.TaggerQL.Parser.Internal

main :: IO ()
main =
    defaultMain
        ( testGroup
            "Test"
            [ normalParsers
            , parserEdgeCases
            , withResource secureResource removeResource $
                \conn ->
                    testGroup
                        "Database Tests"
                        [ setup_0_InitializeDatabase conn
                        , after AllSucceed "Setup 0" $ setup_1_TestInitialization conn
                        ]
            ]
        )

normalParsers :: TestTree
normalParsers =
    testGroup
        "Normal Parsers"
        [ testGroup
            "Criteria"
            ( ( \(a, s) ->
                    testCase
                        (T.unpack s)
                        ( assertEqual
                            ""
                            ( Right
                                ( Request
                                    [ SentenceNode
                                        ( CombinableSentence
                                            Intersect
                                            ( Sentence
                                                [ Simple
                                                    ( SimpleTerm
                                                        (Term a "_")
                                                    )
                                                ]
                                            )
                                        )
                                    ]
                                )
                            )
                            (parse requestParser "test" (s <> "_"))
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
                                ( Request
                                    [ SentenceNode
                                        ( CombinableSentence
                                            Intersect
                                            ( Sentence
                                                [ Simple
                                                    ( SimpleTerm
                                                        (Term MetaDescriptorCriteria "_")
                                                    )
                                                ]
                                            )
                                        )
                                    , SentenceNode
                                        ( CombinableSentence
                                            a
                                            ( Sentence
                                                [ Simple
                                                    ( SimpleTerm
                                                        (Term MetaDescriptorCriteria "_")
                                                    )
                                                ]
                                            )
                                        )
                                    ]
                                )
                            )
                            (parse requestParser "test" ("_ " <> s <> " _"))
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
        , testCase
            "Subquery"
            ( assertEqual
                ""
                ( Right
                    ( Request
                        [ SentenceNode
                            ( CombinableSentence
                                Intersect
                                ( Sentence
                                    [ Complex
                                        ( Term MetaDescriptorCriteria "o%yui"
                                            :<- [ Bottom
                                                    ( Term
                                                        MetaDescriptorCriteria
                                                        "cute"
                                                    )
                                                , Bottom
                                                    ( Term
                                                        DescriptorCriteria
                                                        "smile"
                                                    )
                                                , Term MetaDescriptorCriteria "r%bow"
                                                    :<- [ Bottom
                                                            ( Term
                                                                MetaDescriptorCriteria
                                                                "red"
                                                            )
                                                        ]
                                                ]
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                )
                (parse requestParser "test" "o%yui {cute d.smile r%bow {red} }")
            )
        ]

parserEdgeCases :: TestTree
parserEdgeCases =
    testGroup
        "Parser Edge Cases"
        [ testCase
            "Parse a nested subquery with trailing whitespace"
            ( assertEqual
                ""
                ( Right
                    ( Request
                        [ SentenceNode
                            ( CombinableSentence
                                Intersect
                                ( Sentence
                                    [ Simple
                                        ( SimpleTerm
                                            (Term MetaDescriptorCriteria "otsuki%")
                                        )
                                    ]
                                )
                            )
                        , SentenceBranch
                            Difference
                            [ SentenceNode
                                ( CombinableSentence
                                    Intersect
                                    ( Sentence
                                        [ Simple
                                            ( SimpleTerm
                                                (Term MetaDescriptorCriteria "bruh")
                                            )
                                        ]
                                    )
                                )
                            , SentenceBranch
                                Union
                                [ SentenceNode
                                    ( CombinableSentence
                                        Intersect
                                        ( Sentence
                                            [ Simple
                                                ( SimpleTerm
                                                    (Term MetaDescriptorCriteria "squad")
                                                )
                                            ]
                                        )
                                    )
                                ]
                            ]
                        ]
                    )
                )
                (parse requestParser "test" "otsuki% d|(bruh u| (squad) )")
            )
        , testCase
            "Parse a nested subquery without trailing whitespace."
            ( assertEqual
                ""
                ( Right
                    ( Request
                        [ SentenceNode
                            ( CombinableSentence
                                Intersect
                                ( Sentence
                                    [ Simple
                                        ( SimpleTerm
                                            (Term MetaDescriptorCriteria "otsuki%")
                                        )
                                    ]
                                )
                            )
                        , SentenceBranch
                            Difference
                            [ SentenceNode
                                ( CombinableSentence
                                    Intersect
                                    ( Sentence
                                        [ Simple
                                            ( SimpleTerm
                                                (Term MetaDescriptorCriteria "bruh")
                                            )
                                        ]
                                    )
                                )
                            , SentenceBranch
                                Union
                                [ SentenceNode
                                    ( CombinableSentence
                                        Intersect
                                        ( Sentence
                                            [ Simple
                                                ( SimpleTerm
                                                    (Term MetaDescriptorCriteria "squad")
                                                )
                                            ]
                                        )
                                    )
                                ]
                            ]
                        ]
                    )
                )
                (parse requestParser "test" "otsuki% d|(bruh u| (squad))")
            )
        , testCase
            "Use a parenthetical query in the middle of a query"
            ( assertEqual
                ""
                ( Right
                    ( Request
                        [ SentenceNode
                            ( CombinableSentence
                                Intersect
                                ( Sentence
                                    [ Simple
                                        ( SimpleTerm
                                            (Term MetaDescriptorCriteria "kazuho")
                                        )
                                    ]
                                )
                            )
                        , SentenceNode
                            ( CombinableSentence
                                Union
                                ( Sentence
                                    [ Simple
                                        ( SimpleTerm
                                            (Term MetaDescriptorCriteria "kaede")
                                        )
                                    ]
                                )
                            )
                        , SentenceBranch
                            Union
                            [ SentenceNode
                                ( CombinableSentence
                                    Intersect
                                    ( Sentence
                                        [ Simple
                                            ( SimpleTerm
                                                (Term MetaDescriptorCriteria "%kirari")
                                            )
                                        , Simple
                                            ( SimpleTerm
                                                (Term MetaDescriptorCriteria "hold")
                                            )
                                        , Simple
                                            ( SimpleTerm
                                                (Term MetaDescriptorCriteria "f%anzu")
                                            )
                                        ]
                                    )
                                )
                            ]
                        , SentenceNode
                            ( CombinableSentence
                                Union
                                ( Sentence
                                    [ Complex
                                        ( Term MetaDescriptorCriteria "k%akane"
                                            :<- ( Bottom
                                                    (Term MetaDescriptorCriteria "smile")
                                                    :| []
                                                )
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                )
                ( parse
                    requestParser
                    "test"
                    "kazuho u| kaede u| (%kirari hold f%anzu) u| k%akane {smile}"
                )
            )
        ]