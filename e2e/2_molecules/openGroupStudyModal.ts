/**
 * openGroupStudyModal molecule — click the Group Study button in the study
 * editor header and wait for the #groupStudy modal to become visible.
 *
 * Leaves the browser on the /study/<id> page with the group study modal open.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { OpenGroupStudyModalResult } from '../types/molecules';

export async function openGroupStudyModal(page: Page): Promise<OpenGroupStudyModalResult> {
  const editor = new StudyPageAtoms(page);

  await editor.clickGroupStudy();
  await page.locator('#groupStudy').waitFor({ state: 'visible', timeout: 10_000 });

  return { open: true };
}
