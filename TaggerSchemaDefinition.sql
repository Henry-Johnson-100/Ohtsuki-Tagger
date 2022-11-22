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
  FOREIGN KEY("metaDescriptorId") REFERENCES "Descriptor"("id") 
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  FOREIGN KEY("infraDescriptorId") REFERENCES "Descriptor"("id") 
    ON DELETE CASCADE
    ON UPDATE CASCADE
);
CREATE TABLE IF NOT EXISTS  "Tag" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "fileId" INTEGER NOT NULL,
  "descriptorId" INTEGER NOT NULL,
  "subTagOfId" INTEGER,
  CONSTRAINT "TagUnique" UNIQUE (fileId, descriptorId, subTagOfId) ON CONFLICT IGNORE,
  FOREIGN KEY("fileId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorId") REFERENCES "Descriptor"("id")
    ON DELETE CASCADE
    ON UPDATE CASCADE,
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

-- Triggers

-- -- Descriptor triggers
CREATE TRIGGER IF NOT EXISTS IgnorePrincipleDescriptorDeletions BEFORE DELETE ON Descriptor
  WHEN OLD.descriptor IN ('#ALL#', '#UNRELATED#', '#META#')
    BEGIN
      SELECT RAISE(IGNORE);
    END
;
CREATE TRIGGER IF NOT EXISTS IgnorePrincipleDescriptorUpdates BEFORE UPDATE OF id, descriptor ON Descriptor
  WHEN OLD.descriptor IN ('#ALL#', '#META#', '#UNRELATED#')
    BEGIN
      SELECT RAISE(IGNORE);
    END
;
CREATE TRIGGER IF NOT EXISTS DefaultMetaDescriptorRelation AFTER INSERT ON Descriptor
  BEGIN
    INSERT INTO MetaDescriptor
      SELECT
        (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#' LIMIT 1),
        NEW.id;
  END
;
CREATE TRIGGER IF NOT EXISTS CascadeDescriptorDeletionToRelations AFTER DELETE ON Descriptor
  BEGIN
    INSERT INTO MetaDescriptor
      SELECT
        (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#' LIMIT 1),
        id
      FROM Descriptor
      WHERE id NOT IN (SELECT infraDescriptorId FROM MetaDescriptor);
  END
;

-- -- MetaDescriptor triggers
CREATE TRIGGER IF NOT EXISTS IgnorePrincipleInfraInsert BEFORE INSERT ON MetaDescriptor
  WHEN NEW.infraDescriptorId IN (SELECT id FROM Descriptor WHERE descriptor IN ('#ALL#', '#META#', '#UNRELATED#'))
    BEGIN
      SELECT RAISE(IGNORE);
    END
;
-- This one may be controversial. Absolutely NO updates are allowed on this table.
-- INSERT must be used to change a relationship.
CREATE TRIGGER IF NOT EXISTS NoUpdateOfMetaDescriptor BEFORE UPDATE OF metaDescriptorId, infraDescriptorId ON MetaDescriptor
  BEGIN
    SELECT RAISE(IGNORE);
  END
;
CREATE TRIGGER IF NOT EXISTS BreakRecursiveInsert AFTER INSERT ON MetaDescriptor
  WHEN NEW.infraDescriptorId NOT IN (
    WITH RECURSIVE allTree(id) AS (
      SELECT metaDescriptorId
      FROM MetaDescriptor
      WHERE metaDescriptorId IN (SELECT id FROM Descriptor WHERE descriptor = '#ALL#')
      UNION ALL
      SELECT md.infraDescriptorId
      FROM allTree t
      JOIN MetaDescriptor md ON t.id = md.metaDescriptorId
    )
    SELECT id FROM allTree
  )
  BEGIN
    INSERT INTO MetaDescriptor 
      SELECT 
        (SELECT id FROM Descriptor WHERE descriptor = '#UNRELATED#' LIMIT 1), 
        NEW.infraDescriptorId;
  END
;
INSERT INTO TaggerDBInfo (_tagger, version, lastAccessed)
  VALUES (0, '2.0.0.0', datetime());
