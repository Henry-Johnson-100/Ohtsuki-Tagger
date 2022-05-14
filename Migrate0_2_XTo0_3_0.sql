CREATE TABLE IF NOT EXISTS  "TempTag" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "fileTagId" INTEGER NOT NULL,
  "descriptorTagId" INTEGER NOT NULL,
  "subTagOfId" INTEGER,
  CONSTRAINT "TagKey" UNIQUE("fileTagId", "descriptorTagId", "subTagOfId") 
    ON CONFLICT IGNORE,
  FOREIGN KEY("fileTagId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorTagId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
INSERT INTO TempTag (fileTagId, descriptorTagId, subTagOfId)
  SELECT fileTagId, descriptorTagId, NULL FROM Tag;
DROP TABLE Tag;
CREATE TABLE IF NOT EXISTS  "Tag" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "fileTagId" INTEGER NOT NULL,
  "descriptorTagId" INTEGER NOT NULL,
  "subTagOfId" INTEGER,
  CONSTRAINT "TagKey" UNIQUE("fileTagId", "descriptorTagId", "subTagOfId") 
    ON CONFLICT IGNORE,
  FOREIGN KEY("fileTagId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorTagId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
INSERT INTO Tag (fileTagId, descriptorTagId, subTagOfId)
  SELECT fileTagId, descriptorTagId, NULL FROM TempTag;
DROP TABLE TempTag;