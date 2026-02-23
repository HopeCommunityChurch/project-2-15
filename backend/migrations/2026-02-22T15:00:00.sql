ALTER TABLE "user" ADD COLUMN "emailVerified" boolean NOT NULL DEFAULT TRUE;

CREATE TABLE "user_email_verification" (
  "token"     text        NOT NULL,
  "userId"    uuid        NOT NULL REFERENCES "user" ("userId"),
  "email"     citext      NOT NULL,
  "expiresAt" timestamptz NOT NULL,
  "usedAt"    timestamptz,
  "sentAt"    timestamptz NOT NULL,
  "created"   timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY ("token")
);
