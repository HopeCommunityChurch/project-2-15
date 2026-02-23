-- Add version counter to document table
ALTER TABLE "document" ADD COLUMN "version" integer NOT NULL DEFAULT 0;

-- Persistent step log
CREATE TABLE IF NOT EXISTS "document_step"
  ( "docId"       uuid        NOT NULL
  , "version"     integer     NOT NULL   -- sequential per document (1-based)
  , "step"        jsonb       NOT NULL   -- Step.toJSON() output
  , "userId"      uuid        NOT NULL
  , "computerId"  text        NOT NULL
  , "createdAt"   timestamptz NOT NULL DEFAULT now()
  , PRIMARY KEY ("docId", "version")
  , FOREIGN KEY ("docId")   REFERENCES "document" ("docId")
  , FOREIGN KEY ("userId")  REFERENCES "user"     ("userId")
  );

CREATE INDEX IF NOT EXISTS "idx_document_step_created"
  ON "document_step" ("docId", "createdAt");

-- Periodic full-doc snapshots (every 50 steps)
CREATE TABLE IF NOT EXISTS "document_snapshot"
  ( "docId"      uuid        NOT NULL
  , "atVersion"  integer     NOT NULL
  , "document"   jsonb       NOT NULL
  , "createdAt"  timestamptz NOT NULL DEFAULT now()
  , PRIMARY KEY ("docId", "atVersion")
  , FOREIGN KEY ("docId") REFERENCES "document" ("docId")
  );
