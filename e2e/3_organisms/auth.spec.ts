import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { logout } from '../2_molecules/logout';

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
});
