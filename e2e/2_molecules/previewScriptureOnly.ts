/**
 * previewScriptureOnly molecule — open the Insert Scripture toolbar popup,
 * fill the Bible reference, click Preview, and assert the preview section is
 * visible — without proceeding to insert the passage into the editor.
 *
 * Leaves the browser on the /study/<id> editor page with the scripture popup
 * open and the preview content visible.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { PreviewScriptureOnlyResult } from '../types/molecules';

export async function previewScriptureOnly(
  page: Page,
  ref: string,
): Promise<PreviewScriptureOnlyResult> {
  const editor = new StudyPageAtoms(page);

  await editor.clickInsertScripture();
  await editor.fillScriptureRef(ref);
  await editor.clickScripturePreview();
  // Wait for the preview panel to become visible — confirms the API responded.
  await page.locator('#previewScripture').waitFor({ state: 'visible', timeout: 10_000 });

  return { ref };
}
