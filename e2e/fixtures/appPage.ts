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
import { SignupPageAtoms } from '../1_atoms/signupPage';
import { HomePageAtoms } from '../1_atoms/homePage';
import { ProfilePageAtoms } from '../1_atoms/profilePage';
import { ResetPasswordPageAtoms } from '../1_atoms/resetPasswordPage';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import { GroupStudyInvitePageAtoms } from '../1_atoms/groupStudyInvitePage';
import { createTestUser, type TestUser } from './userFactory';

export { LoginPageAtoms, GlobalAtoms, StudiesPageAtoms, StudyPageAtoms, SignupPageAtoms, HomePageAtoms, ProfilePageAtoms, ResetPasswordPageAtoms, GroupStudyPageAtoms, GroupStudyInvitePageAtoms };

// ── Fixtures ──────────────────────────────────────────────────────────────────

type AtomFixtures = {
  auth: LoginPageAtoms;
  nav: GlobalAtoms;
  studies: StudiesPageAtoms;
  editor: StudyPageAtoms;
  signup: SignupPageAtoms;
  home: HomePageAtoms;
  profile: ProfilePageAtoms;
  resetPassword: ResetPasswordPageAtoms;
  groupStudy: GroupStudyPageAtoms;
  groupInvite: GroupStudyInvitePageAtoms;
  /** A freshly created DB user, unique to this test. Always use this instead of TEST_ACCOUNTS. */
  freshUser: TestUser;
};

export const test = base.extend<AtomFixtures>({
  auth:      async ({ page }, use) => use(new LoginPageAtoms(page)),
  nav:       async ({ page }, use) => use(new GlobalAtoms(page)),
  studies:   async ({ page }, use) => use(new StudiesPageAtoms(page)),
  editor:    async ({ page }, use) => use(new StudyPageAtoms(page)),
  signup:    async ({ page }, use) => use(new SignupPageAtoms(page)),
  home:      async ({ page }, use) => use(new HomePageAtoms(page)),
  profile:       async ({ page }, use) => use(new ProfilePageAtoms(page)),
  resetPassword: async ({ page }, use) => use(new ResetPasswordPageAtoms(page)),
  groupStudy:    async ({ page }, use) => use(new GroupStudyPageAtoms(page)),
  groupInvite:   async ({ page }, use) => use(new GroupStudyInvitePageAtoms(page)),
  freshUser:     async ({}, use) => use(await createTestUser()),
});

export { expect };
