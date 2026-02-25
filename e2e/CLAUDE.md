# E2E Test Suite

## What Exists

Always read the files in `1_atoms/`, `2_molecules/`, and `3_organisms/` before creating anything new.

Key support files:
- `fixtures/appPage.ts` — extends Playwright `test` with atom fixtures; import `test`/`expect` from here, not `@playwright/test`
- `fixtures/userFactory.ts` — `createTestUser()` and the `freshUser`/`secondaryUser` fixtures; use these by default
- `fixtures/credentials.ts` — legacy static accounts (`TEST_ACCOUNTS`); only use for auth-flow tests that need a known stable account
- `fixtures/globalSetup.ts` — runs once: seeds DB from `fixtures/seed.sql`, saves auth state to `fixtures/.auth/*.json`
- `types/atoms.ts` — interfaces for every atom class; update this when adding atom methods
- `types/molecules.ts` — return types for every molecule; update this when adding molecules

---

## Architecture

Tests follow three layers, each building strictly on the one below. **Nothing imports upward.**

### `1_atoms/` — Single Unit
- Single-purpose wrappers around a single page element or interaction — one action or assertion per method.
- Each file covers one page. Global atoms (e.g. nav bar) live in `1_atoms/global.ts`.
- Never use `.isVisible()` as a boolean check.

### `2_molecules/` — Mechanical sequences
- A fixed sequence of atom calls that completes one self-contained interaction. No branching, loops, or recovery.
- Takes `(page: Page, ...inputs)` and returns a plain result object (e.g. `{ studyId, url }`). Never return `{}` — if the molecule knows what it created (index, ID, URL), return it.
- Import atoms from `../1_atoms/X` only. Never include assertions.

### `3_organisms/` — Complete test scenarios
- Each file covers **one feature surface** — named after the feature (`studies.spec.ts`, `editor-basic.spec.ts`), never by action (`create.spec.ts`).
- Split files exceeding ~200 lines by sub-surface (`editor-formatting.spec.ts`, `editor-structure.spec.ts`).
- Wrap tests in `test.describe('<feature>')` matching the filename.
- Flat sequences of atom/molecule calls only — no branches, no recovery, no `test.step()`.
- Never call `expect` directly — route all assertions through atom methods.

---

## Adding to the Suite

### New atom
- Create `1_atoms/<pageName>Page.ts` with class `<PageName>PageAtoms implements I<PageName>PageAtoms`.
- Add the interface to `types/atoms.ts`.
- Register as a fixture in `fixtures/appPage.ts` (import, export, add to `AtomFixtures`, add provider).

### New molecule
- Create `2_molecules/<action>.ts` exporting one async function.
- Import atoms from `../1_atoms/X` only.
- End with an observable state wait (`waitForURL`, `waitFor`, etc.).
- Add a return type to `types/molecules.ts`.

### New organism
- Create `3_organisms/<feature>.spec.ts`.
- Import `test` from `../fixtures/appPage`, molecules by function name.
- Use `randomUUID().slice(0, 8)` in test data titles.
- Add `@smoke` to the test title if it's on the critical path; add to `4_branch_suites/<slug>.json` if it belongs to a feature branch suite.

---

## Credentials and Auth State

Most tests use `freshUser` — it creates a unique DB user per test and logs them in. This is the default.

For multi-user tests, use `secondaryUser` alongside `freshUser`. Static accounts (`credentials.ts`) are legacy — only reach for them when testing auth flows that need a known stable account or a pre-authenticated `storageState`.

If you add, rename, or change a static account password, update **both** `credentials.ts` and `seed.sql`.

---

## Multi-User Tests

Use the `secondaryUser` fixture for tests requiring a second authenticated user. Access it as `{ page, ctx, user }`. The `SecondaryUserContext` type is exported from `fixtures/appPage.ts`.

- Always use `secondaryUser.page` for secondary-user interactions. The primary `page` always belongs to `freshUser`.
- Instantiate atom classes directly on `secondaryUser.page` — do not use fixtures for secondary pages.
- The default `onFailure` screenshot captures only the primary page. Manually attach secondary screenshots on failure (see `testInfo.attach`).

---

## Data Isolation

No database teardown between runs. Tests must:
- Use UUID suffixes on all created data: `const title = \`Study ${randomUUID().slice(0, 8)}\``
- Assert only on data they created — never on global counts
- Never add per-test data to `seed.sql`

---

## Suites

### `4_branch_suites/<slug>.json`
- A curated list of test titles matched to the blast radius of a specific branch. Slug is the branch name with `/` → `-`.
- `scripts/resolve-suite.mjs` selects the correct suite for the current branch, falling back to `--grep @smoke`.

### `@smoke` tag
- Add `@smoke` to a test title to include it in the smoke run (any branch without a dedicated suite).
- Tag only the single most critical happy-path test per feature. Keep the total smoke budget under 5 minutes.

---

## Flaky Tests

Tag with `@flaky` in the test title and add an inline reason comment:

```typescript
test('syncs across tabs @flaky', /* FLAKY: race in WS reconnect */ async ({ page }) => { ... })
```

`npm run check:flaky` fails in CI if any `@flaky` tag exists without a reason comment. Never tag a test flaky just to make it pass — fix the root cause.

---

## Waiting Strategy

Never use `waitForTimeout`. Always wait for observable state. For real-time features, use a tight timeout to encode the SLA (e.g. `{ timeout: 2000 }`). If you must use a timeout, add a brief comment explaining why.

---

## Running Tests

```bash
npm test                                           # branch suite if available, else smoke
npm run test:smoke                                 # smoke suite only
npx playwright test 3_organisms/auth.spec.ts       # single organism
npm run typecheck                                  # TypeScript check without running tests
BASE_URL=https://staging.example.com npm test      # custom target
```
