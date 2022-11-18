{-# LANGUAGE OverloadedStrings #-}

module Test.Text.TaggerQL.Expression.Parser (
  parserTests,
) where

import Data.Either (isLeft)
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
                  , testCase "Pattern 7" (assertBool "" (isLeft (parseP "a&a")))
                  , testCase "Pattern 8" (assertEqual "" (Right "a&a") (parseP "a\\&a"))
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
            "untagged const"
            (assertEqual "" (Right UntaggedConst) (parseExpr "u."))
        , testCase
            "file term value"
            (assertEqual "" (Right (FileTermValue "hello")) (parseExpr "p.hello"))
        , testCase
            "tag expression entrance"
            ( assertEqual
                ""
                ( Right
                    ( TagExpression
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
                        ( Binary
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
                        ( Binary
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
                        ( Binary
                            ( Binary
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
                        ( Binary
                            ( TagTermValue (MetaDescriptorTerm "a")
                            )
                            Union
                            ( Binary
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
                        ( Binary
                            ( Binary
                                ( Binary
                                    (TagTermValue (MetaDescriptorTerm "a"))
                                    Intersect
                                    ( TagExpression
                                        (MetaDescriptorTerm "b")
                                        (SubTag (MetaDescriptorTerm "c"))
                                    )
                                )
                                Union
                                ( Binary
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
                          ( SubExpression
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
                          ( SubExpression
                              (MetaDescriptorTerm "a")
                              ( SubExpression
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
                              ( SubBinary
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
                              ( SubBinary
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
                              ( SubBinary
                                  ( SubBinary
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
                              ( SubBinary
                                  ( SubBinary
                                      ( SubBinary
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
                              ( SubBinary
                                  ( SubBinary
                                      (SubTag (MetaDescriptorTerm "a"))
                                      Union
                                      (SubTag (MetaDescriptorTerm "b"))
                                  )
                                  Intersect
                                  ( SubBinary
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
                              ( SubBinary
                                  ( SubExpression
                                      (MetaDescriptorTerm "a")
                                      (SubTag (MetaDescriptorTerm "b"))
                                  )
                                  Union
                                  ( SubExpression
                                      (MetaDescriptorTerm "c")
                                      ( SubBinary
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
              ]
        )
    ]