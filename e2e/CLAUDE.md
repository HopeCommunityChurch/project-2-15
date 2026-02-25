# E2E Test Suite

## What Exists

Always read the files in `1_atoms/`, `2_molecules/`, and `3_organisms/` before creating anything new.

Key support files:
- `fixtures/appPage.ts` — extends Playwright `test` with atom fixtures; import `test`/`expect` from here, not `@playwright/test`
- `fixtures/credentials.ts` — all test account credentials (`TEST_ACCOUNTS.main`, `.e2eTest`, `.support1–3`)
- `fixtures/globalSetup.ts` — runs once: seeds DB from `fixtures/seed.sql`, saves auth state to `fixtures/.auth/*.json`
- `types/atoms.ts` — interfaces for every atom class; update this when adding atom methods
- `types/molecules.ts` — return types for every molecule; update this when adding molecules
- Add a return type in `types/molecules.ts` for every molecule — never return `void` or `{}`.
  - Return enough context for the caller’s next move:
    - If the molecule knows what it just created (index, ID, URL), return it.
  - Return the minimum useful information needed to act, not a full dump of internal state.
---

## Architecture

Tests follow three layers, each building strictly on the one below. **Nothing imports upward.**

### `1_atoms/` - Single Unit
- Single-purpose wrappers around a single page element or interaction — one action or assertion per method.
- Each file covers one page.
- Atoms that work on every page (e.g. the nav bar) live in `1_atoms/global.ts`.
- All methods return `Promise<void>` — atoms never return data
- Never use .isVisible() as a boolean check

### `2_molecules/` — Mechanical sequences
- A fixed sequence of atom calls
- Complete one self-contained interaction, leaving the browser in a well-defined state. No branching, loops, or recovery: If a step fails, the molecule fails.
- Takes `(page: Page, ...inputs)` and returns a plain result object (e.g. `{ studyId, url }`)
- Import atoms directly from `../1_atoms/X` — never from `../fixtures/appPage`
- Never include assertions

### `3_organisms/` — Complete test scenarios
- Each organism file covers **one feature surface** — one noun the user operates on.
- Name files after the feature (e.g., `studies.spec.ts`, `editor-basic.spec.ts`) — **never** by action (e.g., `create.spec.ts`).
- If a file exceeds ~200 lines, split by **sub-surface** (e.g., `editor-formatting.spec.ts`, `editor-structure.spec.ts`).
- Wrap every organism’s tests in `test.describe('<feature>')` that matches the filename.
- Keep each test as a **flat sequence** of atom and molecule calls — no branches, no recovery logic.
- Import `test` from `../fixtures/appPage` (not `@playwright/test`); import molecules by function name.
- Never call `expect` directly — route all assertions through atom methods.
- Do not add `test.step()` — if a failure is hard to locate without it, the test is doing too much; split it.

---

## Adding to the Suite

### New atom
- create `1_atoms/<pageName>Page.ts` with class `<PageName>PageAtoms implements I<PageName>PageAtoms` (e.g. `1_atoms/loginPage.ts` → `LoginPageAtoms`). 
- Add the interface to `types/atoms.ts` (all methods `Promise<void>`). 
- Register as a fixture in `fixtures/appPage.ts` (import, export, add to `AtomFixtures`, add provider). 
- Document each method with a selector.

### New molecule 
- create `2_molecules/<action>.ts` exporting one async function. 
- Import atoms from `../1_atoms/X` only. 
- Compose a fixed sequence ending with `waitForURL`. 
- Add a return type to `types/molecules.ts`; never return `void`. 
- Add a top comment describing the end state.

### New organism:
- create `3_organisms/<feature>.spec.ts`.
- Import `test` from `../fixtures/appPage`, molecules by function name.
- Use `randomUUID().slice(0, 8)` in test data titles. 
- Add `@smoke` to the test title if it's on the critical path; add to `4_branch_suites/<slug>.json` if it belongs to a feature branch suite.

---

## Import Rules

```typescript
// ❌ Never in an organism or molecule
import { test } from '@playwright/test';        // skips atom fixtures
import { AuthAtoms } from '../fixtures/appPage'; // use appPage only for test/expect
```

The `login` fixture and the `login` molecule share the same name — rename the fixture via destructuring when using both:

```typescript
test('valid credentials redirect', async ({ page, login: loginAtoms }) => {
  await login(page, TEST_EMAIL, TEST_PASSWORD);   // molecule function
  await loginAtoms.assertLoggedIn();              // fixture atom
});
```

---

## Credentials, Seed, and Auth State

These three files form a chain — **all three must stay in sync**:

1. **`fixtures/credentials.ts`** — defines test accounts (`TEST_ACCOUNTS`)
2. **`fixtures/seed.sql`** — inserts matching users into the DB (idempotent: `ON CONFLICT DO NOTHING`)
3. **`fixtures/globalSetup.ts`** — reads credentials, logs in each account, saves session to `fixtures/.auth/*.json`

If you add, rename, or change a password for an account, update **both** `credentials.ts` and `seed.sql`. The `.auth/` files are gitignored and regenerated on every suite run.

Tests that verify auth flows (login, logout, signup) must **not** use cached auth state — they call the `login()` molecule directly. Tests that need a pre-authenticated user should use `test.use({ storageState: 'fixtures/.auth/user.json' })`.

---

## Waiting Strategy

- Never wait for time — always wait for observable state. Never use `waitForTimeout`.
- For real-time features, use a tight timeout to encode the SLA (e.g. `{ timeout: 2000 }`).
- If do you have to use a time out, include a brief, single-line comment explaining why. 

```typescript
// ✅ Inside atoms — assertions auto-retry via expect()
await expect(this.page.getByRole('heading', { name: 'Study' })).toBeVisible();

// ✅ Inside molecules — wait for navigation to complete
await page.waitForURL('**/study/**');
```

---

## Data Isolation

No database teardown between runs. Tests must:
- Use UUID suffixes on all created data: `const title = \`Study ${randomUUID().slice(0, 8)}\``
- Assert only on data they created — never on global counts
- Never add per-test data to `seed.sql`

---

## Multi-User Tests
- Use the `secondaryUser` fixture for tests that require a second authenticated user. It:
    - Creates a fresh DB user
    - Logs them in within an isolated browser context
    - Closes the context automatically after the test
- Access it as `{ page, ctx, user }`:
    - `page` → the secondary user’s browser page
    - `ctx` → their `BrowserContext`
    - `user` → `{ userId, email, password }`
- Always use `secondaryUser.page` for all secondary-user interactions. The primary `page` fixture always belongs to `freshUser`.
- If setup requires the secondary user to be on a specific page, navigate them there before interacting.
- Instantiate atom classes directly on `secondaryUser.page`. Do not use fixtures for secondary pages.
- The default `onFailure` screenshot captures only the primary page. Manually attach secondary screenshots on failure (see `testInfo.attach`).
---

## Suites

**Organisms** answer "what does this feature do?" — they are comprehensive, permanent, and grow over time. **Branch suites** answer "what might this PR have broken?" — they are cross-cutting selections optimized for fast feedback.

### `4_branch_suites/<slug>.json`
- A curated list of test titles drawn from across the organism library, matched to the blast radius of a specific branch.
- For example, a branch touching member roles might pull 3 tests from `group-study.spec.ts`, 2 from `editor-basic.spec.ts`, and 1 from `auth.spec.ts`.
- Slug is the branch name with `/` → `-`.
- These files persist after merge. There is no auto-promotion.
- `scripts/resolve-suite.mjs` selects the correct suite for the current branch, falling back to `-grep @smoke`.

### `@smoke` tag
- Add `@smoke` to a test title to include it in the smoke run. 
- The smoke suite runs on any branch that has no dedicated suite.
- Tag only the single most critical happy-path test per feature.
- Keep the total smoke budget under 5 minutes. Tag sparingly.
   
---

## Flaky Tests

Tag with `@flaky` in the test title and add an inline reason comment:

```typescript
test('syncs across tabs @flaky', /* FLAKY: race in WS reconnect */ async ({ page }) => { ... })
```

`npm run check:flaky` fails in CI if any `@flaky` tag exists without a reason comment. Never tag a test flaky just to make it pass — fix the root cause.

---

## Failure Diagnostics

On failure, Playwright captures screenshots, videos, and traces (see `playwright.config.ts`). Read the trace before doing anything else — it shows DOM state + network timeline at the exact failure point.

The call stack through layers identifies failures without `test.step()`:
```
Error: Locator '.ProseMirror[contenteditable="true"]' not visible
  at EditorAtoms.assertVisible   (1_atoms/editor.ts:13)
  at createStudy                 (2_molecules/createStudy.ts:19)
  at editor-basic.spec.ts:12
```

---

## Running Tests

```bash
npm test                                           # branch suite if available, else smoke
npm run test:smoke                                 # smoke suite only
npx playwright test 3_organisms/auth.spec.ts       # single organism
npm run typecheck                                  # TypeScript check without running tests
BASE_URL=https://staging.example.com npm test      # custom target
```
