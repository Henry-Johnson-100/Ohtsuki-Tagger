{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Module      : Database.Tagger.Script
Description : Scripts for initialization or maintenance of a Tagger database.

License     : GPL-3
Maintainer  : monawasensei@gmail.com
-}
module Database.Tagger.Script (
  SQLiteScript (..),
  schemaDefinition,
  schemaTeardown,
  update0_3_4_0To0_3_4_2,
  patch_2_0,
) where

import Data.String (IsString (..))
import Data.Text (Text)
import Paths_tagger (getDataFileName)
import System.IO (
  IOMode (ReadMode),
  hClose,
  hGetContents,
  openFile,
 )
import Text.RawString.QQ (r)

newtype SQLiteScript = SQLiteScript Text deriving (Show, Eq)

{- |
 Defines the tables of a Tagger database and inserts some
 default information.
-}
schemaDefinition :: IO SQLiteScript
schemaDefinition =
  getScriptContents "TaggerSchemaDefinition.sql"

{- |
 DROPS all tables in a Tagger database.
-}
schemaTeardown :: IO SQLiteScript
schemaTeardown =
  getScriptContents "TaggerSchemaTeardown.sql"

update0_3_4_0To0_3_4_2 :: IO SQLiteScript
update0_3_4_0To0_3_4_2 =
  return
    . SQLiteScript
    . fromString
    $ [r|
    DROP TABLE IF EXISTS Representative;
    |]

patch_2_0 :: IO SQLiteScript
patch_2_0 = getScriptContents "Patch/patch_2_0.sql"

getScriptContents :: FilePath -> IO SQLiteScript
getScriptContents p = do
  scriptFile <- getDataFileName p
  scriptHandle <- openFile scriptFile ReadMode
  scriptContents <- hGetContents scriptHandle
  let !script = SQLiteScript . fromString $ scriptContents
  hClose scriptHandle
  return script
