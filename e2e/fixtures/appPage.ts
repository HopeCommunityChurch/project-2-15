/**
 * Atom fixtures — attaches atom instances to Playwright tests by page slug.
 *
 * Atom classes live in 1_atoms/ — one file per UI area. Each fixture is named
 * after the page slug it primarily operates on.
 *
 * Usage in tests (import from this file, not @playwright/test):
 *
 *   import { test, expect } from '../../fixtures/appPage';
 *
 *   test('example', async ({ login, nav }) => {
 *     await login.fillEmail('user@example.com');
 *     await nav.assertLoggedIn();
 *   });
 *
 * Adding atoms:
 *   1. Create or extend a class in 1_atoms/ (e.g. 1_atoms/studies.ts).
 *   2. Import it here and add a fixture entry below.
 *   3. Document each method with the selector it targets so future authors
 *      know what to update when the UI changes.
 */

import { test as base, expect } from '@playwright/test';
import { AuthAtoms } from '../1_atoms/auth';
import { NavAtoms } from '../1_atoms/nav';
import { StudiesAtoms } from '../1_atoms/studies';
import { EditorAtoms } from '../1_atoms/editor';

export { AuthAtoms, NavAtoms, StudiesAtoms, EditorAtoms };

// ── Fixtures ──────────────────────────────────────────────────────────────────

type AtomFixtures = {
  login: AuthAtoms;
  nav: NavAtoms;
  studies: StudiesAtoms;
  editor: EditorAtoms;
};

export const test = base.extend<AtomFixtures>({
  login:   async ({ page }, use) => use(new AuthAtoms(page)),
  nav:     async ({ page }, use) => use(new NavAtoms(page)),
  studies: async ({ page }, use) => use(new StudiesAtoms(page)),
  editor:  async ({ page }, use) => use(new EditorAtoms(page)),
});

export { expect };
