drop table document_editor;
drop table document;
drop table study;

create table if not exists "group_study"
  ( "groupStudyId" uuid not null default gen_random_uuid()
  , "studyTemplateId" uuid
  , "name" text not null
  , "created" timestamptz not null default now()
  , PRIMARY KEY ("groupStudyId")
  , FOREIGN KEY ("studyTemplateId") REFERENCES study_template ("studyTemplateId")
  );

create table if not exists "group_study_owner"
  ( "groupStudyId" uuid not null
  , "userId" uuid not null
  , PRIMARY KEY ("groupStudyId", "userId")
  , FOREIGN KEY ("groupStudyId") REFERENCES group_study ("groupStudyId")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );

create table if not exists "document"
  ( "docId" uuid not null default gen_random_uuid()
  , "groupStudyId" uuid
  , "studyTemplateId" uuid
  , "name" text not null
  , "document" jsonb not null
  , "created" timestamptz not null default now()
  , "updated" timestamptz not null default now()
  , PRIMARY KEY ("docId")
  , FOREIGN KEY ("groupStudyId") REFERENCES group_study ("groupStudyId")
  , FOREIGN KEY ("studyTemplateId") REFERENCES study_template ("studyTemplateId")
  );

create table if not exists "document_editor"
  ( "docId" uuid not null
  , "userId" uuid not null
  , PRIMARY KEY ("docId", "userId")
  , FOREIGN KEY ("docId") REFERENCES document ("docId")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );
