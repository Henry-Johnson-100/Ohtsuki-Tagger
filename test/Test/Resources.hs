{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.Resources (
  module Test.Resources,
) where

import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes)
import Data.String (fromString)
import qualified Data.Text as T
import Database.YuiTagger (
  Descriptor (Descriptor, descriptor),
  File (File, filePath),
  RecordKey,
  Tag (Tag),
  TaggedConnection,
  allDescriptors,
  allFiles,
  allMetaDescriptorRows,
  allTags,
  close,
  getAllInfra,
  insertDescriptorRelation,
  insertDescriptors,
  insertFiles,
  insertTags,
  openOrCreate,
  queryForSingleDescriptorByDescriptorId,
  teardownDatabase,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  testCase,
  testCaseSteps,
 )
import Test.Tasty.QuickCheck (
  Arbitrary (arbitrary),
  Arbitrary1 (..),
  Arbitrary2 (..),
  Function,
  Gen,
  oneof,
  sized,
  suchThat,
 )
import Text.YuiQL.AST

instance Arbitrary2 LabeledFreeTree where
  liftArbitrary2 :: Gen a -> Gen b -> Gen (LabeledFreeTree a b)
  liftArbitrary2 l a = sized liftSized
   where
    liftSized n
      | n <= 0 = pure <$> a
      | otherwise = Edge <$> liftSized (n `div` 2) <*> l <*> liftSized (n `div` 2)

instance Arbitrary l => Arbitrary1 (LabeledFreeTree l) where
  liftArbitrary :: Arbitrary l => Gen a -> Gen (LabeledFreeTree l a)
  liftArbitrary a = liftArbitrary2 arbitrary a

instance (Arbitrary l, Arbitrary a) => Arbitrary (LabeledFreeTree l a) where
  arbitrary :: (Arbitrary l, Arbitrary a) => Gen (LabeledFreeTree l a)
  arbitrary = liftArbitrary2 arbitrary arbitrary

instance (Function l, Function a) => Function (LabeledFreeTree l a)

instance Arbitrary Pattern where
  arbitrary :: Gen Pattern
  arbitrary = fromString <$> suchThat arbitrary (not . null)

instance Arbitrary1 DTerm where
  liftArbitrary a = oneof [DTerm <$> a, DMetaTerm <$> a]

instance Arbitrary a => Arbitrary (DTerm a) where
  arbitrary :: Arbitrary a => Gen (DTerm a)
  arbitrary = liftArbitrary arbitrary

instance Function a => Function (DTerm a)

instance Arbitrary RingOperation where
  arbitrary :: Gen RingOperation
  arbitrary = oneof $ map pure [(minBound :: RingOperation) .. maxBound]

instance Function RingOperation

instance (Arbitrary1 t, Arbitrary1 k) => Arbitrary1 (FreeDisjunctMonad t k) where
  liftArbitrary ::
    (Arbitrary1 t, Arbitrary1 k) =>
    Gen a ->
    Gen (FreeDisjunctMonad t k a)
  liftArbitrary g = sized liftSized
   where
    liftSized n
      | n <= 0 = PureDisjunct <$> g
      | otherwise =
        oneof
          [ T <$> liftArbitrary (liftSized (n `div` 2))
          , K <$> liftArbitrary (liftSized (n `div` 2))
          ]

instance
  (Arbitrary1 t, Arbitrary1 k, Arbitrary a) =>
  Arbitrary (FreeDisjunctMonad t k a)
  where
  arbitrary ::
    (Arbitrary1 t, Arbitrary1 k, Arbitrary a) =>
    Gen (FreeDisjunctMonad t k a)
  arbitrary = liftArbitrary arbitrary

instance Arbitrary2 TraversableQueryExpression where
  liftArbitrary2 :: Gen a -> Gen b -> Gen (TraversableQueryExpression a b)
  liftArbitrary2 a b = sized liftSized
   where
    liftSized n
      | n <= 0 =
        liftSimpleQueryRing <$> liftArbitrary (liftArbitrary2 a b)
      | otherwise =
        TraversableQueryExpression
          <$> liftArbitrary
            (liftArbitrary2 ((,) <$> liftSized (n `div` 2) <*> b) (liftArbitrary2 a b))

instance Arbitrary a => Arbitrary1 (TraversableQueryExpression a) where
  liftArbitrary :: Arbitrary a => Gen a1 -> Gen (TraversableQueryExpression a a1)
  liftArbitrary b = liftArbitrary2 arbitrary b

instance (Arbitrary a, Arbitrary b) => Arbitrary (TraversableQueryExpression a b) where
  arbitrary :: (Arbitrary a, Arbitrary b) => Gen (TraversableQueryExpression a b)
  arbitrary = liftArbitrary2 arbitrary arbitrary

fe :: Pattern -> QueryExpression
fe = liftSimpleQueryRing . pure . Left

tle :: TagQueryExpression -> QueryExpression
tle = liftSimpleQueryRing . pure . Right

tedp :: DTerm Pattern -> TagQueryExpression
tedp = pure

d :: a -> DTerm a
d = DTerm

rt :: a -> DTerm a
rt = DMetaTerm

secureResource :: IO TaggedConnection
secureResource = openOrCreate "integrated_testing_database.db"

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

{-
A new set of descriptors with prefix "script_" are created by the test
  "descriptorTreeCreateEngineTests" which has a pattern "Descriptor_Tree_Create_Tests"
-}

testTags :: [Tag]
testTags =
  [ -- File_1: Descriptors_ 4
    Tag 1 1 4 Nothing
  , -- File_2: Descriptors_ 5
    Tag 2 2 5 Nothing
  , -- File_3: Descriptors_ 6 7
    Tag 3 3 6 Nothing
  , Tag 4 3 7 Nothing
  , -- File_4: Descriptors_ 5{6}
    Tag 5 4 5 Nothing
  , Tag 6 4 6 (Just 5)
  , -- File_5: Descriptors_ 5{6{7}}
    Tag 7 5 5 Nothing
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
  , {-
      For testing the edge case discussed in
        ticket a50b7d8
    -}
    -- for descriptors 12 meta to 13 and 12 meta to 14
    -- file_8 tagged with 13{15 16}
    Tag 18 8 13 Nothing
  , Tag 19 8 15 (Just 18)
  , Tag 20 8 16 (Just 18)
  , -- file_9 tagged with 14{15 16}
    Tag 21 9 14 Nothing
  , Tag 22 9 15 (Just 21)
  , Tag 23 9 16 (Just 21)
  , -- file_10 tagged with 13{15} 14{16}
    Tag 24 10 13 Nothing
  , Tag 25 10 15 (Just 24)
  , Tag 26 10 14 Nothing
  , Tag 27 10 16 (Just 26)
  , -- End above edge cases
    --
    -- Begin subquery binary operator cases
    -- file_11 tagged with 17{18}
    Tag 28 11 17 Nothing
  , Tag 29 11 18 (Just 28)
  , -- file_12 tagged with 17{19}
    Tag 30 12 17 Nothing
  , Tag 31 12 19 (Just 30)
  , -- file_13 tagged with 17{18 19}
    Tag 32 13 17 Nothing
  , Tag 33 13 18 (Just 32)
  , Tag 34 13 19 (Just 32)
  , -- file_14 tagged with 18{19 20}
    Tag 35 14 18 Nothing
  , Tag 36 14 19 (Just 35)
  , Tag 37 14 20 (Just 35)
  , -- file_15 tagged with 17{18{20}}
    Tag 38 15 17 Nothing
  , Tag 39 15 18 (Just 38)
  , Tag 40 15 20 (Just 39)
  , -- file_16 tagged with 17{18 20}
    Tag 41 16 17 Nothing
  , Tag 42 16 18 (Just 41)
  , Tag 43 16 20 (Just 41)
  , -- Pre-existing tags to test the tagging engine
    -- file_17: 21{22}
    Tag 44 17 21 Nothing
  , Tag 45 17 22 (Just 44)
  {-
    Below are tags that are created during the tagging engine tests
    The tags may not necessarily be in the correct order, but I think they should be
    since the tagging engine works left-to-right on binary expressions.

    Any new, destructive tests written on the database must happen after the pattern:
      "Tagging_Engine_Tests"!

    -- tag file_17 "21{22 23}" to yield new tags:
    Tag 46 17 23 (Just 44)

    -- tag file_17 "21{22{24 25} 23}" to yield new tags:
    Tag 47 17 24 (Just 45)
    Tag 48 17 25 (Just 45)

    -- tag file_18 "26{27{28{29} 30} 31}" to yield new tags:
    Tag 49 18 26 Nothing
    Tag 50 18 27 (Just 49)
    Tag 51 18 28 (Just 50)
    Tag 52 18 29 (Just 51)
    Tag 53 18 30 (Just 50)
    Tag 54 18 31 (Just 49)

    -- tag file_19 "32 33 34"
    Tag 55 19 32 Nothing
    Tag 56 19 33 Nothing
    Tag 57 19 34 Nothing

    -- tag file_19 "33{32}"
    Tag 58 19 32 (Just 56)

    -- tag file_20 "(35 36){37}"
    Tag 59 20 35 Nothing
    Tag 60 20 37 (Just 59)
    Tag 61 20 36 Nothing
    Tag 62 20 37 (Just 61)

    -- tag file_21 "(38 39{40}){41}"
    Tag 63 21 38 Nothing
    Tag 64 21 41 (Just 63)
    Tag 65 21 39 Nothing
    Tag 66 21 40 (Just 65)
    Tag 67 21 41 (Just 66)
  -}
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
newRelations =
  ((newMetaTarget,) <$> newInfraTargets)
    <> [ -- for testing ticket a50b7d8
         (12, 13)
       , (12, 14)
       ]

setup_0_InitializeDatabase :: IO TaggedConnection -> TestTree
setup_0_InitializeDatabase conn =
  testCaseSteps "Initialize_Database" $ \step -> do
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
    "Test_Initialization"
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
    , testCase "All Relations Present" $ do
        actual <- conn >>= allMetaDescriptorRows
        assertEqual
          "Not all relations inserted"
          (length testDescriptors - 1 {-Because #ALL# is not infra to anything-})
          (length actual)
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
