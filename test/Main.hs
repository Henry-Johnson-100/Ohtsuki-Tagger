{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

import Data.List.NonEmpty
import Data.Tagger
import qualified Data.Text as T
import Test.Resources (
    removeResource,
    secureResource,
    setup_0_InitializeDatabase,
    setup_1_TestInitialization,
 )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Text.TaggerQL.Expression.Engine (queryEngineASTTests)
import Test.Text.TaggerQL.Expression.Parser (fParserTests, parserTests)

main :: IO ()
main =
    defaultMain
        ( testGroup
            "Test"
            [ parserTests
            , fParserTests
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
