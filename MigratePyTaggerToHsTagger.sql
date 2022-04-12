PRAGMA foreign_keys = OFF;

BEGIN TRANSACTION;

DROP TABLE IF EXISTS Pytag;
ALTER TABLE Tag RENAME TO Pytag;

-- clearing hanging references
DELETE FROM Image_Tag WHERE Image_ID NOT IN (SELECT Image_ID FROM Image)
  OR Tag_ID NOT IN (SELECT Tag_ID FROM Pytag);
DELETE FROM Related_Tag WHERE Tag_ID NOT IN (SELECT Tag_ID FROM Pytag) 
  OR Related_Tag_ID NOT IN (SELECT Tag_ID FROM Pytag);

-- drop views
DROP VIEW IF EXISTS All_Tag_Relations;
DROP VIEW IF EXISTS Image_With_Tags;

-- define new tables
DROP TABLE IF EXISTS Descriptor;
DROP TABLE IF EXISTS File;
DROP TABLE IF EXISTS MetaDescriptor;
DROP TABLE IF EXISTS Tag;
CREATE TABLE "Descriptor" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "descriptor" VARCHAR NOT NULL,
  CONSTRAINT "DescriptorName" UNIQUE("descriptor") ON CONFLICT IGNORE
);
CREATE TABLE "File" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "filePath" VARCHAR NOT NULL,
  CONSTRAINT "FilePath" UNIQUE("filePath") ON CONFLICT IGNORE
);
CREATE TABLE "MetaDescriptor" (
  "metaDescriptorId" INTEGER NOT NULL,
  "infraDescriptorId" INTEGER NOT NULL,
  CONSTRAINT "InfraRelationUnique" UNIQUE ("infraDescriptorId") ON CONFLICT REPLACE,
  FOREIGN KEY("metaDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE,
  FOREIGN KEY("infraDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
CREATE TABLE "Tag" (
  "fileTagId" INTEGER NOT NULL,
  "descriptorTagId" INTEGER NOT NULL,
  CONSTRAINT "TagKey" UNIQUE("fileTagId", "descriptorTagId") ON CONFLICT IGNORE,
  FOREIGN KEY("fileTagId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorTagId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);

-- populate tables
INSERT INTO Descriptor (id, descriptor)
  SELECT Tag_ID, Tag_Description FROM Pytag;
INSERT INTO File (id, filePath) 
  SELECT Image_ID, Image_Path FROM Image;
INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
  SELECT Tag_ID, Related_Tag_ID FROM Related_Tag;
INSERT INTO Tag (fileTagId, descriptorTagId) 
  SELECT Image_ID, Tag_ID FROM Image_Tag;

-- drop old tables
DROP TABLE IF EXISTS Pytag;
DROP TABLE IF EXISTS Image;
DROP TABLE IF EXISTS Image_Tag;
DROP TABLE IF EXISTS Related_Tag;

-- rename Meta to #META# and add #ALL# and #UNRELATED#
UPDATE Descriptor SET descriptor = '#META#' WHERE descriptor LIKE 'meta';
INSERT INTO Descriptor (descriptor) VALUES ('#ALL#'), ('#UNRELATED#');

-- relate #META# and #UNRELATED# to #ALL#
INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
  SELECT
    (SELECT id FROM Descriptor WHERE descriptor = '#ALL#'),
    id
  FROM Descriptor
  WHERE descriptor IN ('#META#','#UNRELATED#');

-- relate all tags with no relation to #UNRELATED#
INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
  SELECT
    (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#'),
    id
  FROM Descriptor
  WHERE id NOT IN (SELECT infraDescriptorId FROM MetaDescriptor)
    AND id NOT IN (SELECT id FROM Descriptor WHERE descriptor = '#ALL#');

COMMIT;

PRAGMA foreign_keys = ON;