/**
 * applyBoldToText molecule — add a study block, type text into its text cell,
 * select the typed line, apply bold, and wait for the strong element to appear.
 *
 * Uses a study block text cell because ProseMirror only allows the bold mark
 * on paragraph-level nodes (not headings or table headers). Ctrl+A selects
 * mixed content that cannot be uniformly bolded, so we use line selection
 * instead.
 *
 * Leaves the browser on the /study/<id> page with the typed text bolded.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { addStudyBlock } from './addStudyBlock';
import type { ApplyBoldToTextResult } from '../types/molecules';

export async function applyBoldToText(
  page: Page,
  text: string,
): Promise<ApplyBoldToTextResult> {
  const editor = new StudyPageAtoms(page);

  // Add a study block to get a paragraph-level text cell that supports marks.
  await addStudyBlock(page);

  // Click into the text cell of the new block and type.
  await page.locator('.ProseMirror .studyBlocks tr[data-id] td:nth-child(2) p').first().click();
  await page.keyboard.type(text);

  // Select the typed text on the current line, then apply bold.
  await page.keyboard.press('Home');
  await page.keyboard.down('Shift');
  await page.keyboard.press('End');
  await page.keyboard.up('Shift');
  await editor.clickBold();

  // Wait for a strong element containing the typed text to appear in the editor.
  await page.locator('.ProseMirror strong', { hasText: text }).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { text };
}
