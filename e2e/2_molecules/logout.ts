/**
 * logout molecule — open the profile nav from /studies, click Sign Out, and
 * wait for the browser to redirect to the home page.
 *
 * Leaves the browser on the / home page, unauthenticated.
 */

import { Page } from '@playwright/test';
import { StudiesPageAtoms } from '../1_atoms/studiesPage';
import type { LogoutResult } from '../types/molecules';

export async function logout(page: Page): Promise<LogoutResult> {
  const studies = new StudiesPageAtoms(page);

  await studies.clickProfileButton();
  await studies.clickProfileNavSignOut();
  await page.waitForURL('**/', { timeout: 10_000 });

  return { url: page.url() };
}
