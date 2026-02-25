/**
 * closeNewStudyDialog molecule — click the "+ New Study" button to open the
 * new-study dialog, then click the close (X) icon to dismiss it without
 * creating a study, and wait for the dialog to be detached.
 *
 * Assumes the browser is on the /studies page. Leaves the browser on /studies
 * with no new study created.
 */

import { Page } from '@playwright/test';
import { StudiesPageAtoms } from '../1_atoms/studiesPage';
import type { CloseNewStudyDialogResult } from '../types/molecules';

export async function closeNewStudyDialog(page: Page): Promise<CloseNewStudyDialogResult> {
  const studies = new StudiesPageAtoms(page);

  await studies.clickAddStudy();
  // Wait for the dialog to appear before clicking close.
  await page.locator('#newStudy').waitFor({ state: 'visible', timeout: 10_000 });
  await studies.clickCloseNewStudyDialog();
  // Wait for the dialog to be gone.
  await page.locator('#newStudy').waitFor({ state: 'hidden', timeout: 10_000 });

  return { closed: true };
}
