import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { logout } from '../2_molecules/logout';
import { createStudy } from '../2_molecules/createStudy';
import { randomUUID } from 'crypto';

test.describe('auth', () => {
  test('homepage shows Log In button when unauthenticated', async ({ page, nav }) => {
    await page.goto('/');
    await nav.assertLoggedOut();
  });

  test('valid credentials log the user in and land on /app/ @smoke', async ({ page, nav, freshUser }) => {
    await login(page, freshUser.email, freshUser.password);
    await nav.assertLoggedIn();
  });

  test('invalid credentials stay on /login with no redirect', async ({ page, auth, freshUser }) => {
    await page.goto('/login');
    await auth.fillEmail(freshUser.email);
    await auth.fillPassword('wrong-password');
    await auth.clickSubmit();
    await auth.assertOnLoginPage();
    await auth.assertErrorInvalidCredentials();
  });

  test('sign out redirects to the home page', async ({ page, nav, freshUser }) => {
    await login(page, freshUser.email, freshUser.password);
    await logout(page);
    await nav.assertLoggedOut();
  });

  test('user B cannot access user A study URL — sees not-authorized page', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    // User A (freshUser) creates a study.
    await login(page, freshUser.email, freshUser.password);
    const title = `Auth Cross ${randomUUID().slice(0, 8)}`;
    const { studyId } = await createStudy(page, title);

    // User B navigates to User A's study URL — they should not get the editor.
    await secondaryUser.page.goto(`/study/${studyId}`);
    await secondaryUser.page.waitForLoadState('networkidle');
    // The editor must NOT be present (User B has no access).
    // Secondary-user pages don't have fixture access — instantiate atom directly.
    const { StudyPageAtoms } = await import('../1_atoms/studyPage');
    const secondaryEditor = new StudyPageAtoms(secondaryUser.page);
    await secondaryEditor.assertNotVisible();
  });
});
