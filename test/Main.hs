{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty
import Data.Tagger
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL.AST
import Text.TaggerQL.Parser.Internal

main :: IO ()
main = defaultMain (testGroup "Test" [parserEdgeCases])

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
                                        (Term MetaDescriptorCriteria "Otsuki%")
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