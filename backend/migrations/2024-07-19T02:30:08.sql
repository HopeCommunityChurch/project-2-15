create table if not exists "document_save"
  ( "docId" uuid not null
  , "userId" uuid not null
  , "computerId" text not null
  , "time" timestamptz not null
  , PRIMARY KEY ("docId", "computerId", "time")
  , FOREIGN KEY ("docId") REFERENCES "document" ("docId")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );
