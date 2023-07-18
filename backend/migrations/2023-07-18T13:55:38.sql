create table if not exists "study_template"
  ( "studyTemplateId" uuid not null
  , "name" text not null
  , "document" jsonb not null
  , "churchId" uuid not null
  , "created" timestamptz not null default now()
  , PRIMARY KEY ("studyTemplateId")
  , FOREIGN KEY ("churchId") REFERENCES church ("churchId")
  );

create table if not exists "study"
  ( "studyId" uuid not null
  , "studyTemplateId" uuid
  , "name" text not null
  , "created" timestamptz not null default now()
  , PRIMARY KEY ("studyId")
  , FOREIGN KEY ("studyTemplateId") REFERENCES study_template ("studyTemplateId")
  );

create table if not exists "document"
  ( "docId" uuid not null
  , "studyId" uuid not null
  , "name" text not null
  , "document" jsonb not null
  , "created" timestamptz not null default now()
  , PRIMARY KEY ("docId")
  , FOREIGN KEY ("studyId") REFERENCES study ("studyId")
  );

create table if not exists "document_editor"
  ( "docId" uuid not null
  , "userId" uuid not null
  , PRIMARY KEY ("docId", "userId")
  , FOREIGN KEY ("docId") REFERENCES document ("docId")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );
