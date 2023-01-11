{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

import Test.Resources (
    removeResource,
    secureResource,
    setup_0_InitializeDatabase,
    setup_1_TestInitialization,
 )
import Test.Tasty (
    DependencyType (AllSucceed),
    after,
    defaultMain,
    testGroup,
    withResource,
 )
import Test.Text.TaggerQL.Expression.Engine (queryEngineASTTests)
import Test.Text.TaggerQL.Expression.Parser (fParserTests)

main :: IO ()
main =
    defaultMain
        ( testGroup
            "Test"
            [ fParserTests
            , withResource secureResource removeResource $
                \conn ->
                    testGroup
                        "Database Tests"
                        [ setup_0_InitializeDatabase conn
                        , after AllSucceed "Setup 0" $ setup_1_TestInitialization conn
                        , after AllSucceed "Setup" $ queryEngineASTTests conn
                        ]
            ]
        )
