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
      [ testCaseSteps "Initialize Database" $ \step -> do
          step "Inserting test files (file_1..file_100)"
          conn >>= insertFiles (T.unpack . filePath <$> testFiles)

          step "Inserting test descriptors (d_0 .. d_Z)"
          conn >>= insertDescriptors (descriptor <$> newDescriptors)

          step "Inserting Basic Descriptor Relations for Descriptors 4 meta to [5..20]"
          mapM_
            (\(m, i) -> conn >>= insertDescriptorRelation m i)
            newRelations

          step "Tagging some files"
          _ <-
            conn
              >>= insertTags
                (toTagTriple <$> testTags)

          assertBool
            ""
            True
      , after AllSucceed "Initialize Database" $
          testGroup
            "Initialization Tests"
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
      ]