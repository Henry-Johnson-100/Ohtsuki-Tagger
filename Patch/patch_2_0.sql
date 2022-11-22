PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
  CREATE TABLE IF NOT EXISTS  "new_MetaDescriptor" (
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
  INSERT INTO new_MetaDescriptor
    SELECT
      metaDescriptorId,
      infraDescriptorId
    FROM MetaDescriptor;
  DROP TABLE MetaDescriptor;
  ALTER TABLE new_MetaDescriptor RENAME TO MetaDescriptor;

  CREATE TABLE IF NOT EXISTS  "new_Tag" (
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
  INSERT INTO new_Tag
    SELECT
      id,
      fileId,
      descriptorId,
      subTagOfId
    FROM Tag;
  DROP TABLE Tag;
  ALTER TABLE new_Tag RENAME TO Tag;

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
END TRANSACTION;
PRAGMA foreign_keys=ON;