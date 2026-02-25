/**
 * applyItalicToText molecule — add a study block, type text into its text cell,
 * select the typed line, apply italic, and wait for the em element to appear.
 *
 * Mirrors applyBoldToText but for italic. Leaves the browser on the
 * /study/<id> page with the typed text italicised.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { addStudyBlock } from './addStudyBlock';
import type { ApplyItalicToTextResult } from '../types/molecules';

export async function applyItalicToText(
  page: Page,
  text: string,
): Promise<ApplyItalicToTextResult> {
  const editor = new StudyPageAtoms(page);

  // Add a study block to get a paragraph-level text cell that supports marks.
  await addStudyBlock(page);

  // Click into the text cell of the new block and type.
  await page.locator('.ProseMirror .studyBlocks tr[data-id] td:nth-child(2) p').first().click();
  await page.keyboard.type(text);

  // Select the typed text on the current line, then apply italic.
  await page.keyboard.press('Home');
  await page.keyboard.down('Shift');
  await page.keyboard.press('End');
  await page.keyboard.up('Shift');
  await editor.clickItalic();

  // Wait for an em element containing the typed text to appear in the editor.
  await page.locator('.ProseMirror em', { hasText: text }).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { text };
}
