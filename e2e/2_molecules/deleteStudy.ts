/**
 * deleteStudy molecule — open the profile menu on the study editor page,
 * click "Delete Study", confirm, and wait for the browser to redirect to
 * /studies.
 *
 * Leaves the browser on the /studies page.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { DeleteStudyResult } from '../types/molecules';

export async function deleteStudy(page: Page): Promise<DeleteStudyResult> {
  const editor = new StudyPageAtoms(page);

  await editor.clickProfileMenu();
  await editor.clickDeleteStudy();
  await editor.clickConfirmDelete();
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  return { url: page.url() };
}
