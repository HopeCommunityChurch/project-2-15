# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A collaborative document editing platform for Bible study groups. Real-time collaborative editing via WebSockets with OT (Operational Transformation), user authentication, group study management, and ESV Bible API integration.

## Development Setup

Requires Nix with Flakes and Direnv.

The following must be run by the user in an interactive terminal (not Claude):
```bash
devenv up                    # Start PostgreSQL and Nginx (process-compose TUI)
cd frontend && npm run dev   # Start frontend dev server (port 8080)
```

Occasionally run `rm -rf frontend/node_modules frontend/.parcel-cache && cd frontend && npm install` if the frontend has dependency issues.

## Commands

**Frontend:**
```bash
cd frontend
npm run dev    # Dev server with Parcel (hot reload)
npm run build  # Production build to dist/
```

**Backend (Haskell):**
```bash
# Check compilation:
cd backend && nix develop --command cabal build 2>&1
```

**E2E Tests:**
```bash
cd e2e && npm test                    # branch suite if available, else smoke
cd e2e && npm run test:smoke          # smoke suite only
cd e2e && npx playwright test 3_organisms/auth.spec.ts  # single file
cd e2e && npx playwright test --headed  # with visible browser
```

## Architecture

**Stack:**
- Frontend: TypeScript + React, ProseMirror for editing, Parcel bundler, Sass
- Backend: Haskell with Servant (REST/WebSocket) + Scotty (HTMX pages), Beam ORM, Warp server
- Database: PostgreSQL 15
- Infrastructure: Nix + devenv, Nginx reverse proxy

**Two Haskell servers run on separate ports:**
- Port 3000: Scotty — serves server-side HTML via Ginger templates (HTMX flows: login, signup, studies list, study page, document history, profile, password reset)
- Port 3000 also: Servant — REST/WebSocket API at `/api/*` (document real-time collab, Bible verses, ALTCHA captcha)

Both are wired together in `backend/app/Main.hs`.

**Real-time document sync (OT flow):**
1. Editor change → ProseMirror `collab` plugin produces steps → `ws.send({ tag: "Updated", contents: { version, steps, clientId } })`
2. Backend checks version; if matches, inserts steps + bumps version, confirms to sender, broadcasts `DocUpdated` to other editors/listeners; if mismatches, sends `DocConflict` with steps since that version
3. On open, backend sends `DocOpened` with the latest snapshot doc + any pending steps since that snapshot (capped at 50; if > 50 steps, a fresh snapshot is taken instead)
4. `SaveDoc` message triggers a full document save to the `document` column + possible snapshot

**Backend module layout (`backend/lib/`):**
- `Api/Websocket.hs` — WebSocket handler: `InMsg`/`OutMsg` types, per-connection `SocketState`, `DocState` (editors + subscriptions maps), all `handle*` functions
- `Api/Htmx/` — Scotty route handlers (one file per page/feature)
- `Api/Htmx/DocumentHistory.hs` — version history endpoint: groups steps into sessions, reconstructs doc at any version by replaying steps from nearest snapshot
- `Api/Bible.hs` — ESV API proxy
- `Entity/Document.hs` — all DB queries for documents, steps, snapshots (`getStepsSince`, `insertSnapshotIfAbsent`, `maybeTakeSnapshot`, `getLatestSnapshotBefore`)
- `Entity/` — other entities (User, GroupStudy, Shares, Feature, AuthUser)
- `Types.hs` — newtype wrappers for all IDs (UserId, DocId, GroupStudyId, etc.) using the `NewType p a` pattern
- `Database.hs` — Beam schema definition

**Frontend module layout (`frontend/src/`):**
- `index.tsx` — entry point for the document editor page: constructs `MyWebsocket`, initializes `P215Editor` on `DocOpened`, wires all WS events
- `history.tsx` — entry point for the document history page: fetches history groups, renders preview editor with doc reconstructed at selected version
- `WebsocketTypes.tsx` — all WS message types (`SendMsg`/`RecMsg` unions) + `MyWebsocket` class (extends `EventTarget`)
- `Editor/Editor.tsx` — `P215Editor` class: ProseMirror view, `collab` plugin, OT step send/receive, `dispatchSteps` for applying remote steps
- `Editor/textSchema.ts` — ProseMirror schema definition (nodes + marks)
- `Editor/editorUtils.tsx` — ProseMirror commands (bold, italic, heading levels, study blocks)
- `Editor/OtherCursorPlugin.ts` — decorations for remote cursor positions
- `GroupStudy.ts` / `StudyBlockEditor.ts` / `SidebarSections.ts` — group study UI

**Key entities:** User, Document (ProseMirror JSON + OT step log + snapshots), GroupStudy, StudyTemplate, Shares, Feature (feature flags)

**Authentication:** Cookie-based sessions + optional JWT; ALTCHA CAPTCHA on signup; share tokens for group study invitations.

**Database migrations** are in `backend/migrations/` as plain SQL files (timestamp-named).

## OT / Snapshot Model

Documents have a `version` (Int32) column tracking applied step count. The `documentStep` table stores each step as a JSON value with version + clientId. The `documentSnapshot` table stores periodic snapshots keyed by `(docId, version)`. On open, the backend finds the nearest snapshot ≤ current version and sends any steps since it. If the gap exceeds 50 steps, a new snapshot is taken at the current version to keep opens fast.

## Ginger Template Patterns

Templates live in `backend/templates/`. Key constraints:
- `| safe` filter is broken in ginger-0.10.5.2 — do not use it
- To embed JSON in a `<script>` tag, use `toGVal (unsafeRawHtml ...)` with `</script>` escaped:
  ```haskell
  toGVal (unsafeRawHtml (Txt.replace "</script>" "<\\/script>" (Txt.pack (BLC.unpack (Aeson.encode value)))))
  ```

## E2E Test Architecture

Tests follow a strict three-layer pattern (see `e2e/CLAUDE.md` for full details):
- `1_atoms/` — single-element wrappers (actions + assertions, all `Promise<void>`)
- `2_molecules/` — fixed sequences of atoms that return a result object
- `3_organisms/` — `.spec.ts` files; flat sequences of atoms/molecules

Import `test`/`expect` from `../fixtures/appPage`, not from `@playwright/test`. Branch suites live in `4_branch_suites/<branch-slug>.json`; `scripts/resolve-suite.mjs` picks the right one.

Auth state is cached in `fixtures/.auth/*.json` (gitignored); `fixtures/credentials.ts` and `fixtures/seed.sql` must stay in sync.

## Rules
- Don't ever commit or update PRs unless specifically instructed by me or a slash command/skill.
