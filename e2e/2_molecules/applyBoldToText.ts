/**
 * applyBoldToText molecule — type text into the editor, select all content,
 * then apply bold formatting and wait for the strong element to appear.
 *
 * Leaves the browser on the /study/<id> page with the typed text bolded.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { ApplyBoldToTextResult } from '../types/molecules';

export async function applyBoldToText(
  page: Page,
  text: string,
): Promise<ApplyBoldToTextResult> {
  const editor = new StudyPageAtoms(page);

  await editor.typeAtEnd(text);
  // Select all text to apply bold to the typed content.
  await page.keyboard.press('Control+a');
  await editor.clickBold();
  // Wait for a strong element containing the typed text to appear in the editor.
  await page.locator('.ProseMirror strong', { hasText: text }).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { text };
}
