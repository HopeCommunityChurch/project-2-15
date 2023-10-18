create table if not exists "user_password_reset"
  ( "userId" uuid not null
  , "token" text not null
  , "usedAt" timestamptz
  , "expiresAt" timestamptz not null
  , "created" timestamptz not null
  , PRIMARY KEY ("token")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );
