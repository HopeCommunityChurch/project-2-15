/**
 * navigateToSignupFromHome molecule — navigate to the home page and click the
 * Sign Up button in the nav bar, waiting for the browser to reach /signup.
 *
 * Leaves the browser on the /signup page.
 */

import { Page } from '@playwright/test';
import { HomePageAtoms } from '../1_atoms/homePage';
import type { NavigateToSignupFromHomeResult } from '../types/molecules';

export async function navigateToSignupFromHome(page: Page): Promise<NavigateToSignupFromHomeResult> {
  const home = new HomePageAtoms(page);

  await page.goto('/');
  await home.clickNavSignUp();
  await page.waitForURL('**/signup**', { timeout: 10_000 });

  return { url: page.url() };
}
