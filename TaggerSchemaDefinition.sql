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
  CONSTRAINT "TagUnique" UNIQUE (fileId, descriptorId, subTagOfId) ON CONFLICT IGNORE,
  FOREIGN KEY("fileId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE,
  FOREIGN KEY ("subTagOfId") REFERENCES "Tag"("id") ON DELETE CASCADE
);
CREATE TABLE IF NOT EXISTS "TaggerDBInfo" (
  _tagger INTEGER NOT NULL,
  version TEXT NOT NULL,
  lastAccessed TEXT,
  lastBackup TEXT,
  lastAudit TEXT,
  lastClean TEXT,
  CONSTRAINT uniqueInfo UNIQUE(_tagger) ON CONFLICT REPLACE
);

-- Default values required for Tagger
--
-- Note, these are run before triggers are created. Triggers also rely on
-- the presence of these default values.
INSERT INTO Descriptor (descriptor) VALUES ('#ALL#'), ('#META#'),  ('#UNRELATED#');
-- relate #META# and #UNRELATED# to #ALL#
INSERT INTO MetaDescriptor (metaDescriptorId, infraDescriptorId)
  SELECT
    (SELECT id FROM Descriptor WHERE descriptor = '#ALL#'),
    id
  FROM Descriptor
  WHERE descriptor IN ('#META#','#UNRELATED#');

 -- Triggers controlling inserting and updating Meta relations
 --
 -- Note that the number of rows in the MetaDescriptor table must always be equal
 -- to the number of Descriptors in the database. These two triggers are meant to ensure
 -- that by automatically creating default relations for new Descriptors made and for
 -- deleting old references when a new one is inserted.
 --
 -- An INSERT on MetaDescriptor is the same as an UPDATE, and it is recommended that
 -- NO UPDATE clause is allowed to run on MetaDescriptor!
 --
 -- This is important to preserve a continuous tree structure, and to ensure that
 -- the number of rows stays constant.
CREATE TRIGGER IF NOT EXISTS DefaultMetaDescriptorRelation AFTER INSERT ON Descriptor
  BEGIN
    INSERT INTO MetaDescriptor
      SELECT
        (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#' LIMIT 1),
        NEW.id;
  END
;
CREATE TRIGGER IF NOT EXISTS NoSelfRelation AFTER INSERT ON MetaDescriptor
  WHEN NEW.metaDescriptorId = NEW.infraDescriptorId
    AND NEW.metaDescriptorId NOT IN
      (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#' LIMIT 1)
  BEGIN
    UPDATE MetaDescriptor
      SET metaDescriptorId = (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#' LIMIT 1)
      WHERE metaDescriptorId = NEW.metaDescriptorId
    ;
  END
;
CREATE TRIGGER IF NOT EXISTS UnrelatedIsRelatedToAll AFTER INSERT ON MetaDescriptor
  WHEN NEW.infraDescriptorId = (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#' LIMIT 1)
  BEGIN
    UPDATE MetaDescriptor
      SET metaDescriptorId = (SELECT id FROM Descriptor WHERE descriptor = '#ALL#' LIMIT 1)
      WHERE infraDescriptorId = NEW.infraDescriptorId
    ;
  END
;
INSERT INTO TaggerDBInfo (_tagger, version, lastAccessed)
  VALUES (0, '0.3.2.0', datetime());
