/*
Create Hs Tagger Schema

Have to use a temporary Tag table called NewTag
*/
DROP TABLE IF EXISTS "Descriptor";
DROP TABLE IF EXISTS "File";
DROP TABLE IF EXISTS "MetaDescriptor";
DROP TABLE IF EXISTS "NewTag";
CREATE TABLE "Descriptor" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "descriptor" VARCHAR NOT NULL,
  CONSTRAINT "DescriptorName" UNIQUE("descriptor")
);
CREATE TABLE "File" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "filePath" VARCHAR NOT NULL,
  CONSTRAINT "FilePath" UNIQUE("filePath")
);
CREATE TABLE "MetaDescriptor" (
  "metaDescriptorId" INTEGER NOT NULL,
  "infraDescriptorId" INTEGER NOT NULL,
  CONSTRAINT "MetaDescriptorKey" UNIQUE("metaDescriptorId", "infraDescriptorId"),
  FOREIGN KEY("metaDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE,
  FOREIGN KEY("infraDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
CREATE TABLE "NewTag" (
  "fileTagId" INTEGER NOT NULL,
  "descriptorTagId" INTEGER NOT NULL,
  CONSTRAINT "TagKey" UNIQUE("fileTagId", "descriptorTagId"),
  FOREIGN KEY("fileTagId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorTagId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
--Migrate data from Py to Hs
INSERT INTO
  Descriptor (id, descriptor)
SELECT
  Tag_ID,
  Tag_Description
FROM
  Tag;
INSERT INTO
  File (id, filePath)
SELECT
  Image_ID,
  Image_Path
FROM
  Image;
INSERT INTO
  NewTag (fileTagId, descriptorTagId)
SELECT
  Image_ID,
  Tag_ID
FROM
  Image_Tag;
INSERT INTO
  MetaDescriptor (metaDescriptorId, infraDescriptorId)
SELECT
  Tag_ID,
  Related_Tag_ID
FROM
  Related_Tag;
--Drop Py tables
  DROP TABLE Image;
DROP TABLE Image_Tag;
DROP TABLE Related_Tag;
DROP TABLE Tag;
DROP VIEW All_Tag_Relations;
DROP VIEW Image_With_Tags;
-- Recreate Tag, populate and drop NewTag
  CREATE TABLE "Tag" (
    "fileTagId" INTEGER NOT NULL,
    "descriptorTagId" INTEGER NOT NULL,
    CONSTRAINT "TagKey" UNIQUE("fileTagId", "descriptorTagId"),
    FOREIGN KEY("fileTagId") REFERENCES "File"("id") ON DELETE CASCADE,
    FOREIGN KEY("descriptorTagId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
  );
INSERT INTO
  Tag (fileTagId, descriptorTagId)
SELECT
  fileTagId,
  descriptorTagId
FROM
  NewTag;
DROP TABLE NewTag;
-- create new #META# and #ALL# Descriptors
INSERT INTO
  Descriptor (descriptor)
VALUES
  ('#ALL#'),
  ('#META#'),
  ('#UNRELATED#');
-- Replace relations of META x to #META#
INSERT INTO
  MetaDescriptor (metaDescriptorId, infraDescriptorId)
SELECT
  (
    SELECT
      id
    FROM
      Descriptor
    WHERE
      descriptor = '#META#'
  ),
  infraDescriptorId
FROM
  MetaDescriptor
WHERE
  metaDescriptorId = (
    SELECT
      id
    FROM
      Descriptor
    WHERE
      descriptor = 'META'
  );
DELETE FROM
  Descriptor
WHERE
  descriptor = 'META';
-- Delete relations for non-existant tags
DELETE FROM
  MetaDescriptor
WHERE
  metaDescriptorId NOT IN (
    SELECT
      id
    FROM
      Descriptor
  )
  OR infraDescriptorId NOT IN (
    SELECT
      id
    FROM
      Descriptor
  );
-- Relate #ALL# to #META# and #UNRELATED#
INSERT INTO
  MetaDescriptor (metaDescriptorId, infraDescriptorId)
SELECT
  (
    SELECT
      id
    FROM
      Descriptor
    WHERE
      descriptor = '#ALL#'
  ),
  id
FROM
  Descriptor
WHERE
  descriptor IN ('#UNRELATED#', '#META#');
-- Unrelate tags that have more than one relation
DELETE FROM
  MetaDescriptor
WHERE
  infraDescriptorId IN (
    SELECT
      infraDescriptorId
    FROM
      MetaDescriptor
    GROUP BY
      infraDescriptorId
    HAVING
      COUNT(*) > 1
  );
-- Relate #UNRELATED# to tags not infra to anything
INSERT INTO
  MetaDescriptor (metaDescriptorId, infraDescriptorId)
SELECT
  (
    SELECT
      id
    FROM
      Descriptor
    WHERE
      descriptor = '#UNRELATED#'
  ),
  id
FROM
  Descriptor
WHERE
  id NOT IN (
    SELECT
      infraDescriptorId
    FROM
      MetaDescriptor
  )
  AND NOT descriptor = '#ALL#';