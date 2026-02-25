/**
 * navigateToProfile molecule — open the profile nav from /studies, click
 * the Profile link, and wait for the browser to land on /profile.
 *
 * Leaves the browser on the /profile page, still authenticated.
 */

import { Page } from '@playwright/test';
import { StudiesPageAtoms } from '../1_atoms/studiesPage';
import type { NavigateToProfileResult } from '../types/molecules';

export async function navigateToProfile(page: Page): Promise<NavigateToProfileResult> {
  const studies = new StudiesPageAtoms(page);

  await studies.clickProfileButton();
  await studies.clickProfileNavProfile();
  await page.waitForURL('**/profile**', { timeout: 10_000 });

  return { url: page.url() };
}
