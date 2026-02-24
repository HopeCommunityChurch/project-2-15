import { test, expect } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { TEST_ACCOUNTS } from '../fixtures/credentials';

const { email: TEST_EMAIL, password: TEST_PASSWORD } = TEST_ACCOUNTS.e2eTest;

test.describe('login', () => {
  test('homepage shows Log In button when unauthenticated', async ({ page, login: loginAtoms }) => {
    await page.goto('/');
    await loginAtoms.assertLoggedOut();
  });

  test('valid credentials log the user in and land on /app/', async ({ page, login: loginAtoms }) => {
    await login(page, TEST_EMAIL, TEST_PASSWORD);
    await loginAtoms.assertLoggedIn();
  });

  test('invalid credentials stay on /login with no redirect', async ({ page, login: loginAtoms }) => {
    await page.goto('/login');
    await loginAtoms.fillEmail(TEST_EMAIL);
    await loginAtoms.fillPassword('wrong-password');
    await loginAtoms.clickSubmit();
    await expect(page).toHaveURL(/\/login/, { timeout: 5_000 });
  });
});
