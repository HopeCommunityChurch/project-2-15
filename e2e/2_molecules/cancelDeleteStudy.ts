/**
 * cancelDeleteStudy molecule — open the profile menu, click "Delete Study",
 * assert the confirm-delete dialog is visible, then click Cancel and wait for
 * the dialog to disappear.
 *
 * Leaves the browser on the /study/<id> editor page — the study has NOT been
 * deleted.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { CancelDeleteStudyResult } from '../types/molecules';

export async function cancelDeleteStudy(page: Page): Promise<CancelDeleteStudyResult> {
  const editor = new StudyPageAtoms(page);

  await editor.clickProfileMenu();
  await editor.clickDeleteStudy();
  // Wait for the confirm-delete dialog to appear before clicking Cancel.
  await page.locator('#confirmDelete').waitFor({ state: 'visible', timeout: 10_000 });
  await editor.clickCancelDelete();

  // Wait for the confirm-delete dialog to be detached from the DOM.
  await page.locator('#confirmDelete').waitFor({ state: 'hidden', timeout: 10_000 });

  return { url: page.url() };
}
