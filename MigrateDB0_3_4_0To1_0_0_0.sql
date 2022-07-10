DROP TABLE IF EXISTS Representative;
DROP TABLE IF EXISTS TaggerDBInfo;
CREATE TABLE IF NOT EXISTS NewTag (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "fileId" INTEGER NOT NULL,
  "descriptorId" INTEGER NOT NULL,
  "subTagOfId" INTEGER
);
INSERT INTO NewTag (id, fileId, descriptorId, subTagOfId)
  SELECT
    id
    ,fileId
    ,descriptorId
    ,subTagOfId
  FROM Tag
;
DROP TABLE IF EXISTS Tag;
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
INSERT INTO Tag (id, fileId, descriptorId, subTagOfId)
  SELECT
    id
    ,fileId
    ,descriptorId
    ,subTagOfId
  FROM NewTag
;
DROP TABLE IF EXISTS NewTag;
CREATE TABLE IF NOT EXISTS "TaggerDBInfo" (
  _tagger INTEGER NOT NULL,
  version TEXT NOT NULL,
  lastAccessed TEXT,
  lastBackup TEXT,
  lastAudit TEXT,
  lastClean TEXT,
  CONSTRAINT uniqueInfo UNIQUE(_tagger) ON CONFLICT REPLACE
);
INSERT INTO TaggerDBInfo (_tagger, version, lastAccessed)
  VALUES (0, '1.0.0.0', datetime());