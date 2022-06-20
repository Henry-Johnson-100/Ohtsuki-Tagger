import Test.Data.HierarchyMap
import Test.Database.Tagger
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "tagger-lib Tests"
      [ hierarchyMapTests
      , taggerDatabaseTests
      ]