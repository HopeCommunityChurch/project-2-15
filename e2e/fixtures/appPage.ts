/**
 * Atom fixtures — attaches atom instances to Playwright tests by page slug.
 *
 * Atom classes live in 1_atoms/ — one file per page. Each fixture is named
 * after the page slug it primarily operates on.
 *
 * Usage in tests (import from this file, not @playwright/test):
 *
 *   import { test, expect } from '../../fixtures/appPage';
 *
 *   test('example', async ({ auth, nav }) => {
 *     await auth.fillEmail('user@example.com');
 *     await nav.assertLoggedIn();
 *   });
 *
 * Adding atoms:
 *   1. Create or extend a class in 1_atoms/ (e.g. 1_atoms/studiesPage.ts).
 *   2. Import it here and add a fixture entry below.
 *   3. Document each method with the selector it targets so future authors
 *      know what to update when the UI changes.
 */

import { test as base, expect } from '@playwright/test';
import { LoginPageAtoms } from '../1_atoms/loginPage';
import { GlobalAtoms } from '../1_atoms/global';
import { StudiesPageAtoms } from '../1_atoms/studiesPage';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { createTestUser, type TestUser } from './userFactory';

export { LoginPageAtoms, GlobalAtoms, StudiesPageAtoms, StudyPageAtoms };

// ── Fixtures ──────────────────────────────────────────────────────────────────

type AtomFixtures = {
  auth: LoginPageAtoms;
  nav: GlobalAtoms;
  studies: StudiesPageAtoms;
  editor: StudyPageAtoms;
  /** A freshly created DB user, unique to this test. Always use this instead of TEST_ACCOUNTS. */
  freshUser: TestUser;
};

export const test = base.extend<AtomFixtures>({
  auth:      async ({ page }, use) => use(new LoginPageAtoms(page)),
  nav:       async ({ page }, use) => use(new GlobalAtoms(page)),
  studies:   async ({ page }, use) => use(new StudiesPageAtoms(page)),
  editor:    async ({ page }, use) => use(new StudyPageAtoms(page)),
  freshUser: async ({}, use) => use(await createTestUser()),
});

export { expect };
