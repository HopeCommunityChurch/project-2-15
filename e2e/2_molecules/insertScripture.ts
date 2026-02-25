/**
 * insertScripture molecule — open the scripture insert popup, fill in a Bible
 * reference, preview the passage, then insert it into the document.
 *
 * Leaves the browser on the /study/<id> editor page with the scripture node
 * inserted.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { InsertScriptureResult } from '../types/molecules';

export async function insertScripture(
  page: Page,
  ref: string,
): Promise<InsertScriptureResult> {
  const editor = new StudyPageAtoms(page);

  // Capture the count before inserting so we can wait for the (count)-th item to appear.
  const before = await page.locator('.ProseMirror .bibleText').count();

  await editor.clickInsertScripture();
  await editor.fillScriptureRef(ref);
  await editor.clickScripturePreview();
  // Wait for the preview to load before inserting.
  await page.locator('#previewScripture').waitFor({ state: 'visible', timeout: 10_000 });
  await editor.clickScriptureInsert();
  // Wait for the newly inserted bibleText node (index = before) to be visible.
  await page.locator('.ProseMirror .bibleText').nth(before).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { ref };
}
