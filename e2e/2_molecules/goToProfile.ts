/**
 * goToProfile molecule — navigate directly to /profile and wait for the URL
 * to settle.
 *
 * Leaves the browser on the /profile page.
 */

import { Page } from '@playwright/test';
import type { GoToProfileResult } from '../types/molecules';

export async function goToProfile(page: Page): Promise<GoToProfileResult> {
  await page.goto('/profile');
  await page.waitForURL('**/profile**', { timeout: 10_000 });

  return { url: page.url() };
}
