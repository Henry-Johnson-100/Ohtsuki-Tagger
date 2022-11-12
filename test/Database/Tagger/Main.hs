{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Database.Tagger.Main (
  databaseTests,
) where

import Data.List (isInfixOf, sort)
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

databaseTests :: TestTree
databaseTests = withResource secureResource removeResource $
  \conn ->
    testGroup
      "Database Tests"
      [ testCaseSteps "Initialize Database" $ \step -> do
          let testFileNames = ("file_" <>) <$> (show <$> [1 .. 100 :: Int])
              testDescriptors = ("d_" <>) <$> (T.pack . show <$> ['0' .. 'Z'])
              -- Values that get inserted automatically by the schema definition.
              defaultDescriptors =
                [ Descriptor 1 "#ALL#"
                , Descriptor 2 "#META#"
                , Descriptor 3 "#UNRELATED#"
                ]
              defaultRelations =
                [(1, 2), (1, 3)] ::
                  [(RecordKey Descriptor, RecordKey Descriptor)]

          step "Inserting test files (file_1..file_100)"
          conn >>= insertFiles testFileNames

          step "Inserting test descriptors (d_0 .. d_Z)"
          conn >>= insertDescriptors testDescriptors

          step "Inserting Basic Descriptor Relations for Descriptors 4 meta to [5..20]"
          let newRelations = (4,) <$> [5 .. 20]
          mapM_
            (\(m, i) -> conn >>= insertDescriptorRelation m i)
            newRelations

          step "Tagging some files"
          let tagsToInsert =
                [ (1, 4, Nothing) -- 1
                , (2, 5, Nothing) -- 2
                , (3, 6, Nothing) -- 3
                , (3, 7, Nothing) -- 4
                , (4, 5, Nothing) -- 5
                , (4, 6, Just 5) -- 6
                , (5, 5, Nothing) -- 7
                , (5, 6, Just 7) -- 8
                , (5, 7, Just 8) -- 9
                ]
          _ <-
            conn
              >>= insertTags
                tagsToInsert

          step "Testing Inserted Data Counts"
          actualFiles <- conn >>= allFiles
          actualDescriptors <- conn >>= allDescriptors
          actualRelations <- conn >>= allMetaDescriptorRows
          actualTags <- conn >>= allTags
          let actualFilesMatch =
                sort testFileNames == (sort . map (T.unpack . filePath)) actualFiles
              actualDescriptorsMatch =
                sort (testDescriptors ++ map descriptor defaultDescriptors)
                  == (sort . map descriptor) actualDescriptors
              actualRelationsMatch =
                sort (newRelations ++ defaultRelations) == sort actualRelations
              actualTagsMatch =
                sort tagsToInsert
                  == (sort . map (\(Tag _ fid did mstid) -> (fid, did, mstid))) actualTags
          assertBool
            "Database failed to initialize in an expected way"
            ( and
                [ actualFilesMatch
                , actualDescriptorsMatch
                , actualRelationsMatch
                , actualTagsMatch
                ]
            )
      ]