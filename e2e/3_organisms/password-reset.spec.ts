import { test } from '../fixtures/appPage';
import { GlobalAtoms } from '../fixtures/appPage';

import { requestPasswordReset } from '../2_molecules/requestPasswordReset';
import { randomUUID } from 'crypto';
import { Pool } from 'pg';

test.describe('password reset', () => {
  test('requesting reset for unknown email still shows confirmation @smoke', async ({ page, resetPassword }) => {
    const unknownEmail = `unknown-${randomUUID().slice(0, 8)}@nowhere.local`;
    await requestPasswordReset(page, unknownEmail);
    await resetPassword.assertEmailSentConfirmationVisible();
  });

  test('mismatched passwords on reset form shows error', async ({ page, resetPassword }) => {
    // Navigate to the reset form with a fake token via the ?token= query param.
    const fakeToken = randomUUID().replace(/-/g, '');
    await page.goto(`/reset_token?token=${fakeToken}`);
    await page.waitForURL('**/reset_token**', { timeout: 10_000 });
    await resetPassword.fillNewPassword('NewPassword1!');
    await resetPassword.fillRetypePassword('DifferentPassword1!');
    await resetPassword.clickSubmitToken();
    await resetPassword.assertErrorPasswordsMismatch();
  });

  test('password too short on reset form shows error', async ({ page, resetPassword }) => {
    const fakeToken = randomUUID().replace(/-/g, '');
    await page.goto(`/reset_token?token=${fakeToken}`);
    await page.waitForURL('**/reset_token**', { timeout: 10_000 });
    await resetPassword.fillNewPassword('short');
    await resetPassword.fillRetypePassword('short');
    await resetPassword.clickSubmitToken();
    await resetPassword.assertErrorPasswordTooShort();
  });

  test('invalid token on reset form shows invalid token error', async ({ page, resetPassword }) => {
    const fakeToken = randomUUID().replace(/-/g, '');
    await page.goto(`/reset_token?token=${fakeToken}`);
    await page.waitForURL('**/reset_token**', { timeout: 10_000 });
    const newPassword = `ValidPwd1!${randomUUID().slice(0, 4)}`;
    await resetPassword.fillNewPassword(newPassword);
    await resetPassword.fillRetypePassword(newPassword);
    await resetPassword.clickSubmitToken();
    await resetPassword.assertErrorInvalidToken();
  });

  test('reset token can only be used once — second use shows invalid token error', async ({
    page,
    resetPassword,
    freshUser,
  }) => {
    // Insert a valid, non-expired reset token for the freshUser directly into the DB.
    const token = randomUUID().replace(/-/g, '');
    const pool = new Pool({
      host: process.env.PGHOST ?? 'localhost',
      port: parseInt(process.env.PGPORT ?? '5432', 10),
      database: process.env.PGDATABASE ?? 'p215',
      ...(process.env.PGUSER ? { user: process.env.PGUSER } : {}),
      ...(process.env.PGPASSWORD ? { password: process.env.PGPASSWORD } : {}),
    });
    const client = await pool.connect();
    try {
      await client.query(
        `INSERT INTO "user_password_reset" ("userId", "token", "expiresAt", "created")
         VALUES ($1, $2, NOW() + INTERVAL '1 hour', NOW())`,
        [freshUser.userId, token],
      );
    } finally {
      client.release();
      await pool.end();
    }

    const newPassword = `ResetPwd1!${randomUUID().slice(0, 4)}`;

    // First use: reset succeeds.
    await page.goto(`/reset_token?token=${token}`);
    await page.waitForURL('**/reset_token**', { timeout: 10_000 });
    await resetPassword.fillNewPassword(newPassword);
    await resetPassword.fillRetypePassword(newPassword);
    await resetPassword.clickSubmitToken();
    await resetPassword.assertRedirectedAfterReset();

    // Confirm new password works: after reset the app redirects to studies,
    // so we are already authenticated. Just verify we are on the studies page.
    const globalAtoms = new GlobalAtoms(page);
    await globalAtoms.assertLoggedIn();

    // Second use: the token has been consumed — must show invalid token error.
    await page.goto(`/reset_token?token=${token}`);
    await page.waitForURL('**/reset_token**', { timeout: 10_000 });
    await resetPassword.fillNewPassword(newPassword);
    await resetPassword.fillRetypePassword(newPassword);
    await resetPassword.clickSubmitToken();
    await resetPassword.assertErrorInvalidToken();
  });
});
