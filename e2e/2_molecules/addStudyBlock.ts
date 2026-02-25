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

  // Capture the count of existing study block rows before clicking.
  // Individual study blocks are <tr data-id="..."> rows inside .studyBlocks.
  const before = await page.locator('.ProseMirror .studyBlocks tr[data-id]').count();

  await editor.clickAddStudyBlock();

  // Wait for the newly added study block row (index = before) to be visible.
  await page.locator('.ProseMirror .studyBlocks tr[data-id]').nth(before).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { blockIndex: before };
}
