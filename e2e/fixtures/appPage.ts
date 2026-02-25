import { test as base, expect, type Page, type BrowserContext } from '@playwright/test';
import { login } from '../2_molecules/login';
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

export type SecondaryUserContext = { page: Page; ctx: BrowserContext; user: TestUser };

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
  /** A second freshly created DB user on an isolated browser context, for multi-user tests. */
  secondaryUser: SecondaryUserContext;
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
  secondaryUser: async ({ browser }, use) => {
    const user = await createTestUser();
    const ctx = await browser.newContext();
    const page = await ctx.newPage();
    await login(page, user.email, user.password);
    await use({ page, ctx, user });
    await ctx.close();
  },
});

export { expect };
