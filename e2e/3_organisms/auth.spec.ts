import { test, expect } from '../fixtures/appPage';
import { login } from '../2_molecules/login';

test.describe('login', () => {
  test('homepage shows Log In button when unauthenticated', async ({ page, login: loginAtoms }) => {
    await page.goto('/');
    await loginAtoms.assertLoggedOut();
  });

  test('valid credentials log the user in and land on /app/', async ({ page, login: loginAtoms, freshUser }) => {
    await login(page, freshUser.email, freshUser.password);
    await loginAtoms.assertLoggedIn();
  });

  test('invalid credentials stay on /login with no redirect', async ({ page, login: loginAtoms, freshUser }) => {
    await page.goto('/login');
    await loginAtoms.fillEmail(freshUser.email);
    await loginAtoms.fillPassword('wrong-password');
    await loginAtoms.clickSubmit();
    await expect(page).toHaveURL(/\/login/, { timeout: 5_000 });
  });
});
