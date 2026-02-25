/**
 * navigateToLoginFromHome molecule — navigate to the home page and click the
 * "Log In" nav link to reach /login.
 *
 * Leaves the browser on the /login page, unauthenticated.
 */

import { Page } from '@playwright/test';
import { HomePageAtoms } from '../1_atoms/homePage';
import type { NavigateToLoginFromHomeResult } from '../types/molecules';

export async function navigateToLoginFromHome(
  page: Page,
): Promise<NavigateToLoginFromHomeResult> {
  const home = new HomePageAtoms(page);

  await page.goto('/');
  await home.clickNavLogIn();
  await page.waitForURL('**/login**', { timeout: 10_000 });

  return { url: page.url() };
}
