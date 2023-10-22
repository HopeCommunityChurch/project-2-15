create table if not exists "group_study_share"
  ( "groupStudyId" uuid not null
  , "shareToken" text not null
  , "email" text not null
  , "shareAsOwner" boolean not null
  , "expiresAt" timestamptz not null
  , "usedAt" timestamptz
  , "message" text
  , "created" timestamptz not null
  , PRIMARY KEY ("shareToken")
  , FOREIGN KEY ("groupStudyId") REFERENCES "group_study" ("groupStudyId")
  );
