/**
 * login molecule — navigate to /login, fill credentials, submit, assert
 * the app redirects to /app/.
 *
 * Leaves the browser on the /app/ studies page, authenticated as the given user.
 */

import { Page } from '@playwright/test';
import { AuthAtoms } from '../1_atoms/auth';
import type { LoginResult } from '../types/molecules';

export async function login(
  page: Page,
  email: string,
  password: string,
): Promise<LoginResult> {
  const login = new AuthAtoms(page);

  await page.goto('/login');
  await login.fillEmail(email);
  await login.fillPassword(password);
  await login.clickSubmit();
  // Login is a full-page server redirect; first load may hit a cold DB.
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  return { email, url: page.url() };
}
