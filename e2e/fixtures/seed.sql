-- E2E test fixtures — safe to re-run (ON CONFLICT DO NOTHING / DO UPDATE)
--
-- NOTE: Every account seeded here must have a matching entry in fixtures/credentials.ts
-- (email + plaintext password). If you add, remove, or change a password here, update
-- credentials.ts to match — and vice versa.

INSERT INTO "church" ("churchId", "name")
VALUES ('00000000-0000-0000-0000-000000000001', 'E2E Test Church')
ON CONFLICT DO NOTHING;

-- ── Users ─────────────────────────────────────────────────────────────────────

INSERT INTO "user" ("userId", "email", "name", "churchId")
VALUES
  ('00000000-0000-0000-0000-000000000002', 'e2e-test@example.com',   'E2E Test User',      '00000000-0000-0000-0000-000000000001'),
  ('00000000-0000-0000-0000-000000000010', 'jhendrie25@gmail.com',   'James Hendrie',      '00000000-0000-0000-0000-000000000001'),
  ('00000000-0000-0000-0000-000000000011', 'jhendrie25+1@gmail.com', 'James Hendrie (S1)', '00000000-0000-0000-0000-000000000001'),
  ('00000000-0000-0000-0000-000000000012', 'jhendrie25+2@gmail.com', 'James Hendrie (S2)', '00000000-0000-0000-0000-000000000001'),
  ('00000000-0000-0000-0000-000000000013', 'jhendrie25+3@gmail.com', 'James Hendrie (S3)', '00000000-0000-0000-0000-000000000001')
ON CONFLICT DO NOTHING;

-- ── Passwords ─────────────────────────────────────────────────────────────────
-- All hashes generated via Crypto.KDF.BCrypt.hashPassword (cost 10)

-- "TestPassword1!" — for e2e-test@example.com
INSERT INTO "user_password" ("userId", "password")
VALUES ('00000000-0000-0000-0000-000000000002', '$2b$10$A1JuCGUj2U.6MoSs30iwjuMxdHhyE.Cf7Ag78oM0qmExbfAVOjzsW')
ON CONFLICT ("userId") DO UPDATE SET "password" = EXCLUDED."password";

-- "2NqL8QBLXF6cPVX" — for jhendrie25 main + support accounts
INSERT INTO "user_password" ("userId", "password")
VALUES
  ('00000000-0000-0000-0000-000000000010', '$2b$10$O2oXxM5r2qCDr9LpflE9SOGsmyZ8Ii5ba.DT5ir86zsyNMtfuhh8a'),
  ('00000000-0000-0000-0000-000000000011', '$2b$10$O2oXxM5r2qCDr9LpflE9SOGsmyZ8Ii5ba.DT5ir86zsyNMtfuhh8a'),
  ('00000000-0000-0000-0000-000000000012', '$2b$10$O2oXxM5r2qCDr9LpflE9SOGsmyZ8Ii5ba.DT5ir86zsyNMtfuhh8a'),
  ('00000000-0000-0000-0000-000000000013', '$2b$10$O2oXxM5r2qCDr9LpflE9SOGsmyZ8Ii5ba.DT5ir86zsyNMtfuhh8a')
ON CONFLICT ("userId") DO UPDATE SET "password" = EXCLUDED."password";
