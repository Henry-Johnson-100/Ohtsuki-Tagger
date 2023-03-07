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
import Test.Text.TaggerQL.Expression.AST (astTests)
import Test.Text.TaggerQL.Expression.Engine (queryEngineASTTests)
import Test.Text.TaggerQL.Expression.Parser (parserTests)

main :: IO ()
main =
    defaultMain
        ( testGroup
            "Test"
            [ parserTests
            , withResource secureResource removeResource $
                \conn ->
                    testGroup
                        "Database Tests"
                        [ setup_0_InitializeDatabase conn
                        , after AllSucceed "$0 == \"Initialize_Database\"" $
                            setup_1_TestInitialization conn
                        , after AllSucceed "$0 == \"Test_Initialization\"" $
                            queryEngineASTTests conn
                        ]
            , astTests
            ]
        )
