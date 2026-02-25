-- E2E test fixtures — safe to re-run (ON CONFLICT DO NOTHING)
--
-- Seeds the E2E church. Individual test users are created dynamically by
-- fixtures/userFactory.ts — no static accounts are needed here.

INSERT INTO "church" ("churchId", "name")
VALUES ('00000000-0000-0000-0000-000000000001', 'E2E Test Church')
ON CONFLICT DO NOTHING;
