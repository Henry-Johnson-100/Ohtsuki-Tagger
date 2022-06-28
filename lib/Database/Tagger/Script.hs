{-# LANGUAGE QuasiQuotes #-}

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
) where

import Data.String (IsString (..))
import Data.Text (Text)
import Text.RawString.QQ (r)

newtype SQLiteScript = SQLiteScript Text deriving (Show, Eq)

{- |
 Defines the tables of a Tagger database and inserts some
 default information.
-}
schemaDefinition :: SQLiteScript
schemaDefinition =
  SQLiteScript
    . fromString
    $ [r|
PRAGMA foreign_keys = on;
CREATE TABLE IF NOT EXISTS "Descriptor" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "descriptor" VARCHAR NOT NULL,
  CONSTRAINT "DescriptorName" UNIQUE("descriptor") ON CONFLICT IGNORE
);
CREATE TABLE IF NOT EXISTS  "File" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "filePath" VARCHAR NOT NULL,
  CONSTRAINT "FilePath" UNIQUE("filePath") ON CONFLICT IGNORE
);
CREATE TABLE IF NOT EXISTS  "MetaDescriptor" (
  "metaDescriptorId" INTEGER NOT NULL,
  "infraDescriptorId" INTEGER NOT NULL,
  CONSTRAINT "InfraRelationUnique" UNIQUE ("infraDescriptorId") ON CONFLICT REPLACE,
  FOREIGN KEY("metaDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE,
  FOREIGN KEY("infraDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
CREATE TABLE IF NOT EXISTS  "Tag" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "fileId" INTEGER NOT NULL,
  "descriptorId" INTEGER NOT NULL,
  "subTagOfId" INTEGER,
  FOREIGN KEY("fileId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE,
  FOREIGN KEY ("subTagOfId") REFERENCES "Tag"("id") ON DELETE CASCADE
);
CREATE TABLE IF NOT EXISTS "TaggerDBInfo" (
  _tagger INTEGER NOT NULL,
  version TEXT NOT NULL,
  lastAccessed TEXT,
  lastBackup TEXT,
  CONSTRAINT uniqueInfo UNIQUE(_tagger) ON CONFLICT REPLACE
);
INSERT INTO Descriptor (descriptor) VALUES ('#ALL#'), ('#META#'),  ('#UNRELATED#');
-- relate #META# and #UNRELATED# to #ALL#
INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
  SELECT
    (SELECT id FROM Descriptor WHERE descriptor = '#ALL#'),
    id
  FROM Descriptor
  WHERE descriptor IN ('#META#','#UNRELATED#');
INSERT INTO TaggerDBInfo (_tagger, version, lastAccessed)
  VALUES (0, '0.3.2.0', datetime());
|]

{- |
 DROPS all tables in a Tagger database.
-}
schemaTeardown :: SQLiteScript
schemaTeardown =
  SQLiteScript . fromString $
    [r|
      PRAGMA foreign_keys = on;
      DROP TABLE IF EXISTS Tag;
      DROP TABLE IF EXISTS MetaDescriptor;
      DROP TABLE IF EXISTS TaggerDBInfo;
      DROP TABLE IF EXISTS File;
      DROP TABLE IF EXISTS Descriptor;
    |]

update0_3_4_0To0_3_4_2 :: SQLiteScript
update0_3_4_0To0_3_4_2 =
  SQLiteScript . fromString $
    [r|
    DROP TABLE IF EXISTS Representative;
    |]