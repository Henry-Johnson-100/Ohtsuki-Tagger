{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Tagger.Main (
  databaseTests,
) where

import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.Tagger
import Test.Tasty
import Test.Tasty.HUnit
import Text.TaggerQL

secureResource :: IO TaggedConnection
secureResource = open "integrated_testing_database.db"

{- |
 doesn't delete the file but drops all of its contents.
-}
removeResource :: TaggedConnection -> IO ()
removeResource c = do
  teardownDatabase c
  close c

-- Values that get inserted automatically by the schema definition.
defaultDescriptors :: [Descriptor]
defaultDescriptors =
  [ Descriptor 1 "#ALL#"
  , Descriptor 2 "#META#"
  , Descriptor 3 "#UNRELATED#"
  ]

defaultRelations :: [(RecordKey Descriptor, RecordKey Descriptor)]
defaultRelations =
  [(1, 2), (1, 3)] ::
    [(RecordKey Descriptor, RecordKey Descriptor)]

testFiles :: [File]
testFiles = map (\n -> File n ("file_" <> (T.pack . show $ n))) [1 .. 100]

newDescriptors :: [Descriptor]
newDescriptors =
  map
    (\n -> Descriptor n ("descriptor_" <> (T.pack . show $ n)))
    [4 .. 104]

testDescriptors :: [Descriptor]
testDescriptors = newDescriptors <> defaultDescriptors

testTags :: [Tag]
testTags =
  [ Tag 1 1 4 Nothing
  , Tag 2 2 5 Nothing
  , Tag 3 3 6 Nothing
  , Tag 4 3 7 Nothing
  , Tag 5 4 5 Nothing
  , Tag 6 4 6 (Just 5)
  , Tag 7 5 5 Nothing
  , Tag 8 5 6 (Just 7)
  , Tag 9 5 7 (Just 8)
  , {-
      For testing TaggerQL edge cases related to Tech-note f02a13240b,
    -}
    -- file_6 tagged with 8{9{10}}
    Tag 10 6 8 Nothing
  , Tag 11 6 9 (Just 10)
  , Tag 12 6 10 (Just 11)
  , -- file_7 tagged with 8{9} 11{9{10}}
    Tag 13 7 8 Nothing
  , Tag 14 7 11 Nothing
  , Tag 15 7 9 (Just 13)
  , Tag 16 7 9 (Just 14)
  , Tag 17 7 10 (Just 16)
  ]

toTagTriple ::
  Tag ->
  (RecordKey File, RecordKey Descriptor, Maybe (RecordKey Tag))
toTagTriple (Tag _ fid did mstid) = (fid, did, mstid)

newMetaTarget :: RecordKey Descriptor
newMetaTarget = 4

newInfraTargets :: [RecordKey Descriptor]
newInfraTargets = [5 .. 20]

newRelations :: [(RecordKey Descriptor, RecordKey Descriptor)]
newRelations = (newMetaTarget,) <$> newInfraTargets

testRelations :: [(RecordKey Descriptor, RecordKey Descriptor)]
testRelations = newRelations <> defaultRelations

databaseTests :: TestTree
databaseTests = withResource secureResource removeResource $
  \conn ->
    testGroup
      "Database Tests"
      [ setup_0_InitializeDatabase conn
      , after AllSucceed "Setup 0" $ setup_1_TestInitialization conn
      , after AllSucceed "Setup" $ basicQueryFunctionality conn
      , after AllSucceed "Setup" $ queryEdgeCases conn
      ]

setup_0_InitializeDatabase :: IO TaggedConnection -> TestTree
setup_0_InitializeDatabase conn =
  testCaseSteps "Setup 0 - Initialize Database" $ \step -> do
    step "Inserting test files"
    conn >>= insertFiles (T.unpack . filePath <$> testFiles)

    step "Inserting test descriptors"
    conn >>= insertDescriptors (descriptor <$> newDescriptors)

    step "Inserting Basic Descriptor Relations"
    mapM_
      (\(m, i) -> conn >>= insertDescriptorRelation m i)
      newRelations

    step "Tagging files"
    _ <-
      conn
        >>= insertTags
          (toTagTriple <$> testTags)

    assertBool
      ""
      True

setup_1_TestInitialization :: IO TaggedConnection -> TestTree
setup_1_TestInitialization conn =
  testGroup
    "Setup 1 - Test Initialization"
    [ testCase
        "All Test Files Inserted"
        ( do
            actualFiles <- conn >>= allFiles
            assertEqual
              "Failed to insert test files"
              (HS.fromList testFiles)
              (HS.fromList actualFiles)
        )
    , testCase
        "All Test Descriptors Inserted"
        ( do
            actualDescriptors <- conn >>= allDescriptors
            assertEqual
              "Failed to insert test descriptors"
              (HS.fromList testDescriptors)
              (HS.fromList actualDescriptors)
        )
    , testCase
        "New Relations Inserted Properly"
        ( do
            actual <- conn >>= getAllInfra newMetaTarget
            corrTestData <-
              conn
                >>= ( \c ->
                        fmap catMaybes
                          . mapM
                            ( runMaybeT
                                . flip
                                  queryForSingleDescriptorByDescriptorId
                                  c
                            )
                          $ (newMetaTarget : newInfraTargets)
                    )
            assertEqual
              "Test relations not inserted correctly."
              (HS.fromList corrTestData)
              (HS.fromList actual)
        )
    , testCase
        "All Test Tags Inserted"
        ( do
            actual <- conn >>= allTags
            assertEqual
              "Failed to insert test tags"
              (HS.fromList testTags)
              (HS.fromList actual)
        )
    ]

basicQueryFunctionality :: IO TaggedConnection -> TestTree
basicQueryFunctionality conn =
  testGroup
    "Basic Queries"
    [ testCase "Pattern Wildcard" $ do
        r <- conn >>= taggerQL (TaggerQLQuery "p.%")
        a <- conn >>= allFiles
        assertEqual "p.% should retrieve all files in the database" (HS.fromList a) r
    , testCase "Subtag Search 0" $ do
        r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_5 {d.descriptor_6}")
        assertEqual
          "d.descriptor_5 {d.descriptor_6}"
          [File 4 "file_4", File 5 "file_5"]
          r
    ]

queryEdgeCases :: IO TaggedConnection -> TestTree
queryEdgeCases conn =
  testGroup
    "Query Edge Cases"
    [ testGroup
        "Tech-note f02a13240b for files, A: a{b{c}} and B: a{b} d{b{c}}"
        [ testCase
            "Return A for query a{b{c}}"
            $ do
              r <-
                conn
                  >>= taggerQL
                    (TaggerQLQuery "d.descriptor_8 {d.descriptor_9 {d.descriptor_10}}")
              assertEqual
                ""
                (HS.singleton $ File 6 "file_6")
                r
        , testCase "Return A, B for a{b}" $ do
            r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_8 {d.descriptor_9}}")
            assertEqual
              ""
              [File 6 "file_6", File 7 "file_7"]
              r
        , testCase "Return A, B for b{c}" $ do
            r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_9 {d.descriptor_10}")
            assertEqual
              ""
              [File 6 "file_6", File 7 "file_7"]
              r
        , testCase "Return B for d{b}" $ do
            r <- conn >>= taggerQL (TaggerQLQuery "d.descriptor_11 {d.descriptor_9}")
            assertEqual
              ""
              [File 7 "file_7"]
              r
        ]
    ]