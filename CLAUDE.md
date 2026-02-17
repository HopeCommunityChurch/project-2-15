# CLAUDE.md

## 01 — Project Overview

- **P215** is a collaborative Bible study platform (p215.church) where users create, edit, and share structured study documents in real time
- Users belong to churches; church elders manage study templates. Users create group studies from templates, add documents, and collaborate via a rich text editor with scripture integration
- Core workflows: user auth (signup/login/password-reset), study template management, group study creation & sharing, real-time collaborative document editing, Bible passage insertion (ESV API)

**Runtime components:**
- **Backend (Haskell):** REST API (Servant on Warp, port 3000) + HTMX/HTML server (Scotty) running concurrently. WebSocket support for real-time editing
- **Frontend (TypeScript):** Vanilla TS app built on ProseMirror for the document editor. Bundled with Parcel (port 3001 in dev)
- **Nginx:** Reverse proxy unifying both under port 8080 locally (`/api/` → backend, `/` → frontend, `/mailpit/` → mail UI)
- **PostgreSQL 15:** Primary data store (database: `p215`, user: `p215_user`)
- **Mailpit:** Local SMTP server for email testing (port 1025 SMTP, 8025 web UI)

## 02 — Tech Stack

### Languages & versions
- **Haskell** — GHC2021 standard, built with Cabal 3.0 (`backend/backend.cabal`)
- **TypeScript** — ESNext target, CommonJS modules (`frontend/tsconfig.json`)
- **Nix** — nixpkgs release-25.05 (`flake.nix`, `backend/flake.nix`, `frontend/flake.nix`, `ops/flake.nix`)

### Frameworks & key libraries
- **Backend:** Servant (REST API types), Scotty (HTMX routes), Warp (HTTP server), Beam (PostgreSQL ORM), postgresql-simple, websockets/wai-websockets, lucid (HTML), ginger (templates), aeson (JSON), smtp-mail/mime-mail (email), crypton (password hashing), openapi3/servant-swagger-ui (API docs)
- **Frontend:** ProseMirror (editor core — model, view, state, commands, history, keymap, menu), ts-pattern (pattern matching), sass (SCSS), uuid

### Tooling
- **Package managers:** Cabal (backend), npm (frontend), Nix flakes (everything)
- **Dev environment:** devenv + direnv (`.envrc` at root loads `flake.nix`)
- **Build:** `watchexec` for backend hot-reload (`backend/run.sh`), Parcel for frontend bundling
- **Deployment:** Colmena (NixOS cluster management), GitHub Actions CI/CD (`ops/`)
- **Linters/formatters:** GHC `-Wall -Werror=missing-fields -Werror=incomplete-uni-patterns -Wunused-packages` (no separate linter/formatter configured)

### Infrastructure
- **PostgreSQL 15** — configured in `flake.nix` (devenv) and `backend/local-secrets.json`
- **Mailpit** — local email testing, configured in `flake.nix`
- **ESV Bible API** — external service, token in secrets file
- **Altcha** — CAPTCHA service, key in secrets file
- **Production hosting:** DigitalOcean (dev: 178.128.133.233 / dev.p215.church, prod: 157.230.223.205 / p215.church)

### Local prerequisites
- [ ] Nix with flakes enabled
- [ ] direnv (+ optionally nix-direnv)
- [ ] That's it — Nix provides PostgreSQL, Nginx, Mailpit, Haskell tooling, and Node.js

## 03 — Build & Run Commands

### First-time setup (per machine)
```bash
# 1. Install Nix with flakes enabled, and direnv with shell hook
# 2. From repo root — allow direnv
direnv allow
# 3. Deny the frontend's broken direnv (it provides Node 14 via dream2nix, too old for Parcel)
cd frontend && direnv deny && cd ..
# 4. Install frontend dependencies (must use system Node >= 16, NOT the Nix-provided one)
cd frontend && npm install && cd ..
```

### Run locally (dev mode)
```bash
# Terminal 1 (repo root): Start backend, PostgreSQL, Nginx, Mailpit, Hoogle
devenv up

# Terminal 2 (frontend/): Start frontend dev server
cd frontend
npm run dev
```
Then open http://127.0.0.1:8080

### Troubleshooting startup
- **`Attaching to existing process-compose server`** then exits: stale socket from a previous session.
  Fix: `rm /tmp/devenv-*/pc.sock && devenv up`
- **`parcel: command not found` or `SyntaxError: ??=`**: the frontend's dream2nix direnv is active, providing Node 14.
  Fix: `cd frontend && direnv deny && npm install && npm run dev`
- **Frontend node_modules empty after direnv loads**: dream2nix shell hook wipes them.
  Fix: deny direnv, then `npm install` again

### Build frontend for production
```bash
# From frontend/
npm run build
```

### Backend rebuild (happens automatically via watchexec, but manually):
```bash
# From backend/
cabal run -O0
```

### Database
- **Auto-created** by devenv (database: `p215`, user: `p215_user`, password: `password`)
- **Migrations** run automatically on backend startup from `backend/migrations/`
- **Connection:** `127.0.0.1:5432` (see `backend/local-secrets.json`)

### Environment / secrets
- Backend config: `backend/local-secrets.json` (checked into repo with dev defaults)
- Production secrets: managed via GitHub Actions secrets + `SECRETS_FILE` env var
- No `.env.example` — configuration is in the JSON secrets file

### Reset frontend (if node_modules get stale)
```bash
# From frontend/
rm -rf node_modules .parcel-cache .direnv && cd .. && cd frontend/
```

### Deploy
```bash
# Dev deploy (pushes to main branch, or manually):
cd ops && make

# Prod deploy:
cd ops && ./prod-deploy.sh
```

## 04 — Test Commands

- **No test suite exists** for either frontend or backend
- Backend has QuickCheck in dependencies but no test target in `backend/backend.cabal`
- Frontend `package.json` test script is a placeholder: `echo "Error: no test specified" && exit 1`
- **CI/CD** (`/.github/workflows/main.yml`, `production.yml`): builds and deploys only — no test step
- TODO: No coverage gates or quality checks confirmed

## 05 — Architecture Overview

### High-level component diagram
```
Browser (port 8080)
  │
  ├─ /           → Nginx → Frontend (Parcel dev server, port 3001)
  ├─ /api/       → Nginx → Backend  (Warp + Scotty, port 3000)
  ├─ /api/document/realtime → WebSocket (real-time editing)
  └─ /mailpit/   → Nginx → Mailpit  (port 8025)
                              │
                    PostgreSQL 15 (port 5432, db: p215)
```

### Key directories
| Path | Purpose |
|------|---------|
| `backend/app/Main.hs` | Server entry point — config loading, migration, server startup |
| `backend/lib/` | All backend library code |
| `backend/lib/Api.hs` | Main Servant API type definition and server wiring |
| `backend/lib/Api/Auth.hs` | Cookie-based session auth (`p215-auth` cookie) |
| `backend/lib/Api/Bible.hs` | ESV Bible API integration |
| `backend/lib/Api/Websocket.hs` | Real-time document collaboration via WebSocket |
| `backend/lib/Api/Htmx/` | Scotty HTMX routes (Login, Signup, Study, Profile, etc.) |
| `backend/lib/Database.hs` | Beam ORM table definitions (14 tables) |
| `backend/lib/Entity/` | Domain entities with scoped queries (User, AuthUser, GroupStudy, Document, Shares, Feature) |
| `backend/lib/Entity.hs` | `Entity` typeclass — abstracts DB-to-domain mapping |
| `backend/lib/DbHelper.hs` | Connection pool management, transaction helpers |
| `backend/lib/Types.hs` | Core newtypes (UserId, DocId, etc.), token generation, Feature enum |
| `backend/lib/Emails/` | Email templates (Welcome, PasswordReset, ShareGroupStudy) |
| `backend/migrations/` | 12 SQL migration files (2023-07 to 2024-09), run on startup |
| `backend/templates/` | Ginger HTML templates for HTMX-served pages |
| `backend/static/` | Static assets (CSS, images, favicon) served by backend |
| `frontend/src/` | All frontend source (~13 TS files) |
| `frontend/src/index.tsx` | Frontend entry — WebSocket init, document loading, save logic |
| `frontend/src/Editor/Editor.tsx` | Main `P215Editor` class (large file, ~50KB) |
| `frontend/src/Editor/textSchema.ts` | ProseMirror schema (section, bibleText, questions, studyBlocks, etc.) |
| `frontend/src/WebsocketTypes.tsx` | WebSocket message types and `MyWebsocket` class |
| `ops/` | Deployment config — Colmena, NixOS modules, deploy scripts |
| `ops/network/flake.nix` | NixOS server configuration (Nginx, systemd service, firewall) |
| `designStuff/types.md` | Original domain type specifications |

### Data flow
1. **Page load:** Browser hits Nginx → HTMX routes (Scotty) serve server-rendered HTML pages with auth checks
2. **Document editing:** Frontend loads at `/study/{docId}`, opens WebSocket to `/api/document/realtime`, receives document JSON, initializes ProseMirror editor
3. **Real-time sync:** Editor changes → debounced save (1s) → WebSocket `SaveDoc` → backend persists to PostgreSQL JSONB → broadcasts `Updated` to other clients
4. **Bible insertion:** User searches passage → frontend calls `/api/bible/esv?q=...` → backend proxies to ESV API → parsed verses inserted into ProseMirror document
5. **Sharing:** Owner shares group study via email → backend sends invite with token → recipient clicks link → `group_study_share` record consumed

### Structural conventions
- **Backend entity pattern:** Each domain concept has a module in `Entity/` with query types (`Get*`, `New*`, `Update*`) and an `Entity` typeclass instance for scoped DB queries
- **Auth scoping:** Queries are scoped by authenticated user's church — users only see their church's data
- **Database:** Beam ORM with typed table definitions in `Database.hs`; raw SQL migrations in `backend/migrations/` (timestamp-named)
- **Frontend:** No framework — vanilla TS with ProseMirror. DOM manipulation via direct API calls. State managed in `P215Editor` class
- **Document storage:** Rich documents stored as JSONB in PostgreSQL `document` table

### Gotchas
- **Two web frameworks:** Backend runs Servant (REST/WebSocket) and Scotty (HTMX pages) concurrently in the same process — see `Main.hs`
- **Frontend is NOT React/Vue/etc.** — it's vanilla TypeScript with ProseMirror and direct DOM manipulation
- **Backend has its own Nix flake** (`backend/flake.nix`) separate from root flake — the root `devenv up` process runs `nix develop` inside `backend/` to get the Haskell toolchain
- **Frontend also has its own Nix flake** (`frontend/flake.nix`) — must `cd frontend` for npm commands
- **No tests exist** — no test suite, no CI test step
- **GHC flags are strict:** `-Werror=missing-fields -Werror=incomplete-uni-patterns` — incomplete patterns and missing record fields are compile errors
- **30+ GHC extensions enabled by default** in the cabal file (OverloadedStrings, OverloadedRecordDot, NoFieldSelectors, DataKinds, TypeFamilies, etc.)
- **Migrations run on every backend startup** — be careful with destructive SQL
- **`local-secrets.json` is committed** — it contains only dev credentials
- **CSS lives in `backend/static/styles/`**, not in the frontend directory
