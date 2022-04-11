PRAGMA foreign_keys = on;
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
  CONSTRAINT "InfraRelationUnique" UNIQUE ("infraDescriptorId") ON CONFLICT REPLACE,
  FOREIGN KEY("metaDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE,
  FOREIGN KEY("infraDescriptorId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);
CREATE TABLE "Tag" (
  "fileTagId" INTEGER NOT NULL,
  "descriptorTagId" INTEGER NOT NULL,
  CONSTRAINT "TagKey" UNIQUE("fileTagId", "descriptorTagId"),
  FOREIGN KEY("fileTagId") REFERENCES "File"("id") ON DELETE CASCADE,
  FOREIGN KEY("descriptorTagId") REFERENCES "Descriptor"("id") ON DELETE CASCADE
);