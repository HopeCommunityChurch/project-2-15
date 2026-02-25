/**
 * undoChange molecule — click the Undo toolbar button and wait for the editor
 * to reflect the change (confirmed by the editor remaining visible and
 * interactive after the action).
 *
 * Leaves the browser on the /study/<id> editor page with the last change
 * undone.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { UndoChangeResult } from '../types/molecules';

export async function undoChange(page: Page): Promise<UndoChangeResult> {
  const editor = new StudyPageAtoms(page);

  await editor.clickUndo();

  // Wait for the editor to remain interactive — confirms ProseMirror has
  // processed the undo transaction without crashing.
  await page.locator('.ProseMirror[contenteditable="true"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { applied: true };
}
