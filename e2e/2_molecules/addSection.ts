/**
 * addSection molecule — click the "Add Section" button in the left sidebar
 * and wait for a new section item to appear.
 *
 * Leaves the browser on the /study/<id> page with the new section added.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { AddSectionResult } from '../types/molecules';

export async function addSection(page: Page): Promise<AddSectionResult> {
  const editor = new StudyPageAtoms(page);

  // Capture the count before clicking so we can wait for the (count)-th item to appear.
  const before = await page.locator('#leftSidebar .section-item').count();

  await editor.clickAddSection();

  // Wait for the newly added section item (index = before) to be visible.
  await page.locator('#leftSidebar .section-item').nth(before).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return {};
}
