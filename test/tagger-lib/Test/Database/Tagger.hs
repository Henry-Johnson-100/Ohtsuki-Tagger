{-# LANGUAGE OverloadedStrings #-}

{-# HLINT ignore "Use camelCase" #-}

module Test.Database.Tagger (
  taggerDatabaseTests,
) where

import qualified Data.HashSet as HS
import qualified Data.Text as T
import Database.Tagger
import Test.Tasty
import Test.Tasty.HUnit

testDbPath :: FilePath
testDbPath = "test/resources/test_database_tagger.db.rsc"

getConnection :: IO TaggedConnection
getConnection = do
  tc <- open testDbPath
  initializeDatabase tc
  teardownDatabase tc
  initializeDatabase tc
  return tc

freeConnection :: TaggedConnection -> IO ()
freeConnection tc = do
  teardownDatabase tc
  close tc

taggerDatabaseTests :: TestTree
taggerDatabaseTests =
  withResource
    getConnection
    freeConnection
    ( \iotc ->
        testGroup
          "database_tests"
          [ database_initialization iotc
          , after
              AllSucceed
              "database_initialization"
              (dummy_insertions iotc)
          , after
              AllSucceed
              "dummy_insertions"
              ( database_validation
                  iotc
              )
          ]
    )

database_validation :: IO TaggedConnection -> TestTree
database_validation iotc =
  testGroup
    "database_validation"
    [ testCase
        "has_unrelated_descriptors"
        ( iotc
            >>= ( \tc -> do
                    unrel <- getInfraChildren 3 tc
                    assertEqual
                      "#UNRELATED# Does not have the correct number of infra relations\n\
                      \it should contain x, y, and z"
                      3
                      (HS.size unrel)
                )
        )
    , testCase
        "has_untagged_files"
        ( iotc
            >>= ( \tc -> do
                    untagged <- queryForUntaggedFiles tc
                    assertEqual
                      "The number of untagged files is incorrect"
                      6
                      (HS.size untagged)
                )
        )
    , testCase
        "flat_query_for_0_depth_tag"
        ( iotc
            >>= ( \tc -> do
                    taggedWithXXY <- flatQueryForFileByTagDescriptor "xxy" tc
                    assertBool
                      "Could not query for files a and b tagged with xxy"
                      ( HS.member (File 1 "a") taggedWithXXY
                          && HS.member (File 2 "b") taggedWithXXY
                          && HS.size taggedWithXXY == 2
                      )
                )
        )
    , testCase
        "flat_query_for_n_depth_tag"
        ( iotc
            >>= ( \tc -> do
                    subtaggedWithZ <- flatQueryForFileByTagDescriptor "z" tc
                    assertBool
                      "Could not query for files a and b tagged with z"
                      ( HS.member (File 1 "a") subtaggedWithZ
                          && HS.member (File 2 "b") subtaggedWithZ
                          && HS.size subtaggedWithZ == 2
                      )
                )
        )
    , testCase
        "query_for_0_1_depth_subtag_relation"
        ( iotc
            >>= ( \tc -> do
                    f <- queryForFileBySubTagRelation 10 6 tc
                    assertBool
                      "Could not query for files a and b tagged with z"
                      ( HS.member (File 1 "a") f
                          && HS.member (File 2 "b") f
                          && HS.size f == 2
                      )
                )
        )
    , testCase
        "query_for_n_n+1_depth_subtag_relation"
        ( iotc
            >>= ( \tc -> do
                    f <- queryForFileBySubTagRelation 6 7 tc
                    assertBool
                      "Could not query for files a and b tagged with z"
                      ( HS.member (File 2 "b") f && HS.size f == 1
                      )
                )
        )
    ]

dummy_insertions :: IO TaggedConnection -> TestTree
dummy_insertions iotc =
  testGroup
    "dummy_insertions"
    [ testGroup
        "dummy_files_and_descriptors"
        [ testCase
            "dummy_files"
            ( iotc
                >>= ( \tc -> do
                        insertFiles
                          ["a", "b", "c", "d", "e", "f", "g", "h"]
                          tc
                        numFiles <- HS.size <$> allFiles tc
                        assertEqual
                          "Not all files were able to be added."
                          8
                          numFiles
                    )
            )
        , testCase
            "dummy_descriptors"
            ( iotc
                >>= ( \tc -> do
                        insertDescriptors
                          -- ids starting at 4
                          ["x", "y", "z", "xx", "xxy", "yy", "yyz"]
                          tc
                        numDes <- HS.size <$> allDescriptors tc
                        assertEqual
                          "Not all descriptors were able to be added"
                          10
                          numDes
                    )
            )
        ]
    , after AllSucceed "dummy_descriptors" $
        testCase
          "dummy_relations"
          ( iotc
              >>= ( \tc -> do
                      insertDescriptorRelation tc 4 7
                      insertDescriptorRelation tc 7 8
                      insertDescriptorRelation tc 5 9
                      insertDescriptorRelation tc 9 10
                      numRel <- HS.size <$> allMetaDescriptorRows tc
                      assertEqual
                        "Not all relations created properly."
                        9
                        numRel
                  )
          )
    , after AllSucceed "dummy_files_and_descriptors" $
        testCase
          "dummy_tags"
          ( iotc
              >>= ( \tc -> do
                      insertTags
                        [ (1, 8, Nothing)
                        , (1, 10, Nothing)
                        , (1, 6, Just 2)
                        , (2, 8, Nothing)
                        , (2, 10, Nothing)
                        , (2, 6, Just 5)
                        , (2, 7, Just 6)
                        ]
                        tc
                      numTags <- HS.size <$> allTags tc
                      assertEqual
                        "Not all tags created successfully."
                        7
                        numTags
                  )
          )
    ]

database_initialization :: IO TaggedConnection -> TestTree
database_initialization iotc =
  testCase
    "database_initialization"
    ( iotc
        >>= ( \tc -> do
                numFiles <- HS.size <$> allFiles tc
                numDes <- HS.size <$> allDescriptors tc
                numRel <- HS.size <$> allMetaDescriptorRows tc
                numTags <- HS.size <$> allTags tc
                assertEqual
                  "Database initialized with \
                  \incorrect number of expected records."
                  (0, 3, 2, 0)
                  (numFiles, numDes, numRel, numTags)
            )
    )