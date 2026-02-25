/**
 * requestPasswordReset molecule — navigate to /resetpassword, fill the email
 * field, click Send, and wait for the confirmation message to appear.
 *
 * Leaves the browser on the /resetpassword page with the confirmation text
 * visible.
 */

import { Page } from '@playwright/test';
import { ResetPasswordPageAtoms } from '../1_atoms/resetPasswordPage';
import type { RequestPasswordResetResult } from '../types/molecules';

export async function requestPasswordReset(
  page: Page,
  email: string,
): Promise<RequestPasswordResetResult> {
  const resetPage = new ResetPasswordPageAtoms(page);

  await page.goto('/resetpassword');
  await page.waitForURL('**/resetpassword**', { timeout: 10_000 });

  await resetPage.fillEmail(email);
  await resetPage.clickSendResetEmail();
  // Wait for the HTMX swap that replaces the form with the confirmation text.
  await page.locator(':text("Check your email")').waitFor({ state: 'visible', timeout: 10_000 });

  return { email };
}
