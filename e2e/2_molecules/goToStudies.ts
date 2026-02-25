/**
 * goToStudies molecule — navigate directly to /studies and wait for the URL
 * to settle.
 *
 * Leaves the browser on the /studies page.
 */

import { Page } from '@playwright/test';
import type { GoToStudiesResult } from '../types/molecules';

export async function goToStudies(page: Page): Promise<GoToStudiesResult> {
  await page.goto('/studies');
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  return { url: page.url() };
}
