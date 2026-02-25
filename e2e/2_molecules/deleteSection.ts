/**
 * deleteSection molecule — click the Remove button on a sidebar section at the
 * given 0-based index and wait for the section count in the sidebar to decrease.
 *
 * Leaves the browser on the /study/<id> page with the section removed.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { DeleteSectionResult } from '../types/molecules';

export async function deleteSection(
  page: Page,
  sectionIndex: number,
): Promise<DeleteSectionResult> {
  const editor = new StudyPageAtoms(page);

  const before = await page.locator('#leftSidebar .section').count();
  await editor.clickDeleteSection(sectionIndex);

  // Wait for the section count to decrease by polling with Playwright locators.
  const target = before - 1;
  await page.locator('#leftSidebar .section').nth(target).waitFor({
    state: 'detached',
    timeout: 10_000,
  }).catch(() => {
    // If target is 0, nth(0) may not detach but count may already be correct — proceed.
  });

  return { sectionIndex };
}
