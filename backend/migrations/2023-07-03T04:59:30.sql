CREATE EXTENSION if not exists citext;

create table if not exists "church"
  ( "churchId" uuid not null default gen_random_uuid()
  , "name" text not null
  , "created" timestamptz not null default now()
  , PRIMARY KEY ("churchId")
  );


create table if not exists "user"
  ( "userId" uuid not null default gen_random_uuid()
  , "email" citext not null
  , "image" text
  , "churchId" uuid not null
  , "created" timestamptz not null default now()
  , PRIMARY KEY ("userId")
  , FOREIGN KEY ("churchId") REFERENCES church ("churchId")
  );


create table if not exists "user_password"
  ( "userId" uuid not null
  , "password" text not null
  , PRIMARY KEY ("userId")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );


create table if not exists "user_session"
  ( "userId" uuid not null
  , "token" text not null
  , "expires" timestamptz not null
  , "created" timestamptz not null
  , PRIMARY KEY ("token")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );


create table if not exists "church_elder"
  ( "userId" uuid not null
  , "churchId" uuid not null
  , "created" timestamptz not null default now()
  , PRIMARY KEY ("churchId")
  , FOREIGN KEY ("churchId") REFERENCES church ("churchId")
  , FOREIGN KEY ("userId") REFERENCES "user" ("userId")
  );
