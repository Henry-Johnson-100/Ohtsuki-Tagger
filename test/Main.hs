{-# LANGUAGE OverloadedStrings #-}

import Data.Tagger
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL
import Text.TaggerQL.AST
import Text.TaggerQL.Parser.Internal

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
    ]