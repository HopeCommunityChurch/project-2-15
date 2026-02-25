/**
 * navigateToProfileFromEditor molecule — on the study editor page, open the
 * profile nav menu and click the Profile link, waiting for the browser to
 * reach /profile.
 *
 * Leaves the browser on the /profile page.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { NavigateToProfileFromEditorResult } from '../types/molecules';

export async function navigateToProfileFromEditor(
  page: Page,
): Promise<NavigateToProfileFromEditorResult> {
  const editor = new StudyPageAtoms(page);

  await editor.clickProfileMenu();
  // The Profile link inside #profileNav navigates to /profile.
  await page.locator('#profileNav').getByRole('link', { name: 'Profile' }).click();
  await page.waitForURL('**/profile**', { timeout: 10_000 });

  return { url: page.url() };
}
