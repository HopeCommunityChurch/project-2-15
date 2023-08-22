alter table "document" add column "updated" timestamptz not null default now();

