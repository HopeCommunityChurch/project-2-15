/**
 * addStudyBlock molecule — click the "Add Study Block" toolbar button and
 * wait for the study block node to appear in the ProseMirror editor.
 *
 * Leaves the browser on the /study/<id> page with a new study block inserted.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { AddStudyBlockResult } from '../types/molecules';

export async function addStudyBlock(page: Page): Promise<AddStudyBlockResult> {
  const editor = new StudyPageAtoms(page);

  // Capture the count before clicking so we can wait for the (count)-th item to appear.
  const before = await page.locator('.ProseMirror .generalStudyBlock').count();

  await editor.clickAddStudyBlock();

  // Wait for the newly added study block (index = before) to be visible.
  await page.locator('.ProseMirror .generalStudyBlock').nth(before).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { blockIndex: before };
}
