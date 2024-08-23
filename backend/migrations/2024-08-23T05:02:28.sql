create table if not exists "user_feature"
  ( "userId" uuid not null
  , "feature" text not null
  , PRIMARY KEY ("userId", "feature")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );
