# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture Overview

**project-2-15** is a collaborative Bible study platform. Key components:

- **Backend** (`backend/`): Haskell REST API + WebSocket server (Warp/Scotty, PostgreSQL via Beam ORM, Ginger templates for server-rendered pages)
- **Frontend** (`frontend/`): TypeScript/React SPA with ProseMirror rich-text editor
- **E2E Tests** (`e2e/`): Playwright test suite — see `e2e/CLAUDE.md` for full architecture
- **Ops** (`ops/`): Nix flake deployment config
- **Nginx** (port 8080): Reverse proxy routing `/api/` → backend (3000) and `/` → frontend (3001/1234)

## Dev Environment

The dev environment runs via `devenv up` (Nix + process-compose). See memory notes for startup quirks.

Key ports: **8080** (Nginx entry), **3000** (backend), **3001/1234** (frontend Parcel), **5432** (PostgreSQL), **1025/8025** (Mailpit)

## Commands

### Backend
```bash
# Run backend (within nix develop):
cd backend && watchexec --exts hs -r cabal run -O0
# Or via run.sh
```

### Frontend
```bash
cd frontend
npm run dev       # Parcel dev server
npm run build     # Production build
npm run typecheck # TypeScript type check
```

### E2E Tests
```bash
# From repo root:
npm run e2e:seed                              # Seed the E2E database
npm run e2e:test                              # Run branch suite or smoke fallback
npm run e2e:test:smoke                        # Smoke suite only

# From e2e/:
npm test                                      # Branch suite or @smoke fallback
npm run test:smoke                            # Smoke only
npx playwright test 3_organisms/auth.spec.ts  # Single organism
npm run typecheck                             # Type-check without running
BASE_URL=https://staging.example.com npm test # Custom target
```

Dynamic `freshUser` accounts (created per-test via `userFactory.ts`) use password `TestPassword1!`.

To manually create a test account (e.g. for exploratory testing in the browser), run:
```bash
PGHOST=localhost PGPORT=5432 PGDATABASE=p215 /opt/homebrew/opt/libpq/bin/psql -c "
INSERT INTO \"user\" (\"userId\", \"email\", \"name\", \"churchId\")
  VALUES (gen_random_uuid(), 'me@example.com', 'Test User', '00000000-0000-0000-0000-000000000001');
INSERT INTO \"user_password\" (\"userId\", \"password\")
  SELECT \"userId\", '\$2b\$10\$A1JuCGUj2U.6MoSs30iwjuMxdHhyE.Cf7Ag78oM0qmExbfAVOjzsW'
  FROM \"user\" WHERE \"email\" = 'me@example.com';
"
```
Replace `me@example.com` with your desired email. Password will be `TestPassword1!`.

### Full typecheck (root)
```bash
npm run typecheck  # Runs frontend + e2e typechecks
```

## Backend Structure

```
backend/lib/
  Entity/          # Data models (User, Document, GroupStudy, Shares, Feature)
  Api/             # REST endpoints + WebSocket handlers
  Api/Htmx/        # Server-rendered pages (Studies, Study, Profile, Auth, etc.)
  Database.hs      # Beam ORM setup
  DbHelper.hs      # Query helpers
  Password.hs      # bcrypt hashing
  Mail.hs          # Email sending (via Mailpit in dev)
  Bible/           # ESV Bible parsing
backend/migrations/ # SQL migration files
backend/static/     # Ginger HTML templates
```

## Frontend Structure

```
frontend/src/
  index.tsx         # React app entry point
  Editor/           # ProseMirror editor (formatting, scripture, structure)
  GroupStudy.ts     # Group study UI logic
  WebsocketTypes.tsx# Real-time sync types
  Types.tsx         # Shared types
```

## Ginger Templates (Backend)

Embedding raw JSON in `<script>` tags requires `toGVal (unsafeRawHtml ...)` from `Text.Ginger.Html`. The `| safe` filter is broken in this version — do not use it. See memory notes for the correct pattern.
