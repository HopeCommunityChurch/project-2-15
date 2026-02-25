/**
 * signOutFromProfile molecule — on the /profile page, open the profile nav
 * dialog and click Sign Out, waiting for the redirect to the home page.
 *
 * Leaves the browser on the / home page, unauthenticated.
 */

import { Page } from '@playwright/test';
import { ProfilePageAtoms } from '../1_atoms/profilePage';
import type { SignOutFromProfileResult } from '../types/molecules';

export async function signOutFromProfile(page: Page): Promise<SignOutFromProfileResult> {
  const profile = new ProfilePageAtoms(page);

  await profile.clickProfileButton();
  await profile.clickSignOut();
  await page.waitForURL('**/', { timeout: 10_000 });

  return { url: page.url() };
}
