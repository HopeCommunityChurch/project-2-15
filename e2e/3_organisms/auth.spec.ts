import { test, expect } from '../fixtures/appPage';
import { login } from '../2_molecules/login';

test.describe('login', () => {
  test('homepage shows Log In button when unauthenticated', async ({ page, auth }) => {
    await page.goto('/');
    await auth.assertLoggedOut();
  });

  test('valid credentials log the user in and land on /app/', async ({ page, auth, freshUser }) => {
    await login(page, freshUser.email, freshUser.password);
    await auth.assertLoggedIn();
  });

  test('invalid credentials stay on /login with no redirect', async ({ page, auth, freshUser }) => {
    await page.goto('/login');
    await auth.fillEmail(freshUser.email);
    await auth.fillPassword('wrong-password');
    await auth.clickSubmit();
    await expect(page).toHaveURL(/\/login/, { timeout: 5_000 });
  });
});
