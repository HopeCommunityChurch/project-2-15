import { test } from '../fixtures/appPage';
import { randomUUID } from 'crypto';

test.describe('signup', () => {
  test('duplicate email shows email already taken error', async ({ page, signup, freshUser }) => {
    // freshUser already exists in the DB — attempting to sign up with the same
    // email must show the "Email already taken" validation error.
    await page.goto('/signup');
    await page.waitForURL('**/signup**', { timeout: 10_000 });
    await signup.fillName('Duplicate User');
    await signup.fillEmail(freshUser.email);
    await signup.fillPassword('TestPassword1!');
    await signup.fillRetypePassword('TestPassword1!');
    await signup.clickSubmit();
    await signup.assertErrorEmailTaken();
  });

  test('password too short on signup form shows error', async ({ page, signup }) => {
    const email = `signup-short-${randomUUID().slice(0, 8)}@e2e.local`;
    await page.goto('/signup');
    await page.waitForURL('**/signup**', { timeout: 10_000 });
    await signup.fillName('Short Pass User');
    await signup.fillEmail(email);
    await signup.fillPassword('short');
    await signup.fillRetypePassword('short');
    await signup.clickSubmit();
    await signup.assertErrorPasswordTooShort();
  });

  test('mismatched passwords on signup form shows error', async ({ page, signup }) => {
    const email = `signup-mismatch-${randomUUID().slice(0, 8)}@e2e.local`;
    await page.goto('/signup');
    await page.waitForURL('**/signup**', { timeout: 10_000 });
    await signup.fillName('Mismatch User');
    await signup.fillEmail(email);
    await signup.fillPassword('TestPassword1!');
    await signup.fillRetypePassword('DifferentPassword1!');
    await signup.clickSubmit();
    await signup.assertErrorPasswordsMismatch();
  });
});
