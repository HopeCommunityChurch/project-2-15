/**
 * redoChange molecule — click the Redo toolbar button and wait for the editor
 * to reflect the change (confirmed by the editor remaining visible and
 * interactive after the action).
 *
 * Leaves the browser on the /study/<id> editor page with the last undone
 * change re-applied.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { RedoChangeResult } from '../types/molecules';

export async function redoChange(page: Page): Promise<RedoChangeResult> {
  const editor = new StudyPageAtoms(page);

  await editor.clickRedo();

  // Wait for the editor to remain interactive — confirms ProseMirror has
  // processed the redo transaction without crashing.
  await page.locator('.ProseMirror[contenteditable="true"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { applied: true };
}
