import Test.Data.HierarchyMap (hierarchyMapTests)
import Test.Database.Tagger (taggerDatabaseTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Text.TaggerQL.Parser ( queryParserTests )

main :: IO ()
main =
  defaultMain $
    testGroup
      "tagger-lib Tests"
      [ hierarchyMapTests
      , taggerDatabaseTests
      , queryParserTests
      ]