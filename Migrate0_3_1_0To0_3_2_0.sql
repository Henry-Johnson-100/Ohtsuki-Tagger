CREATE TABLE IF NOT EXISTS  "NewTag" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "fileTagId" INTEGER NOT NULL,
  "descriptorTagId" INTEGER NOT NULL,
  "subTagOfId" INTEGER,
  CONSTRAINT "TagKey" UNIQUE("fileTagId", "descriptorTagId", "subTagOfId") 
    ON CONFLICT IGNORE,
  FOREIGN KEY("fileTagId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorTagId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
INSERT INTO TempTag (id, fileTagId, descriptorTagId, subTagOfId)
  SELECT id, fileTagId, descriptorTagId, subTagOfId FROM Tag;
DROP TABLE Tag;
CREATE TABLE IF NOT EXISTS  "Tag" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "fileId" INTEGER NOT NULL,
  "descriptorId" INTEGER NOT NULL,
  "subTagOfId" INTEGER,
  CONSTRAINT "TagKey" UNIQUE("fileId", "descriptorId", "subTagOfId") 
    ON CONFLICT IGNORE,
  FOREIGN KEY("fileId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE,
  FOREIGN KEY ("subTagOfId") REFERENCES "Tag"("id") ON DELETE CASCADE
);
INSERT INTO Tag (id, fileId, descriptorId, subTagOfId)
  SELECT id, fileTagId, descriptorTagId, subTagOfId FROM NewTag;
DROP TABLE NewTag;
CREATE TABLE IF NOT EXISTS "TaggerDBInfo" (
  _tagger INTEGER NOT NULL,
  version TEXT NOT NULL,
  lastAccessed TEXT,
  CONSTRAINT uniqueInfo UNIQUE(_tagger) ON CONFLICT REPLACE
);
INSERT INTO TaggerDBInfo (_tagger, version, lastAccessed)
  VALUES (0, '0.3.2.0', datetime());