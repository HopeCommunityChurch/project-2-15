/**
 * closeGroupStudyModal molecule — click the close icon inside the group study
 * modal and wait for the modal to become hidden.
 *
 * Assumes the group study modal is already open. Leaves the browser on the
 * /study/<id> page with the modal closed.
 */

import { Page } from '@playwright/test';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { CloseGroupStudyModalResult } from '../types/molecules';

export async function closeGroupStudyModal(page: Page): Promise<CloseGroupStudyModalResult> {
  const groupStudy = new GroupStudyPageAtoms(page);

  await groupStudy.clickClose();
  await page.locator('#groupStudy').waitFor({ state: 'hidden', timeout: 10_000 });

  return { closed: true };
}
