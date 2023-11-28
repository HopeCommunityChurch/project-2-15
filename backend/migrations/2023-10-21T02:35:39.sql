create table if not exists "group_study_share"
  ( "groupStudyId" uuid not null
  , "shareToken" text not null
  , "email" text not null
  , "shareAsOwner" boolean not null
  , "expiresAt" timestamptz not null
  , "usedAt" timestamptz
  , "message" text
  , "rejected" boolean not null
  , "created" timestamptz not null
  , PRIMARY KEY ("shareToken")
  , unique ("groupStudyId", "email")
  , FOREIGN KEY ("groupStudyId") REFERENCES "group_study" ("groupStudyId")
  );
