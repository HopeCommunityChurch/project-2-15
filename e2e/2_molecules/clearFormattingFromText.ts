/**
 * clearFormattingFromText molecule — add a study block, type text, bold it,
 * then select the line and click Clear Formatting — waiting for the bold mark
 * to be removed.
 *
 * Leaves the browser on the /study/<id> page with the text present but without
 * any inline marks.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { addStudyBlock } from './addStudyBlock';
import type { ClearFormattingFromTextResult } from '../types/molecules';

export async function clearFormattingFromText(
  page: Page,
  text: string,
): Promise<ClearFormattingFromTextResult> {
  const editor = new StudyPageAtoms(page);

  // Add a study block to get a paragraph-level text cell that supports marks.
  await addStudyBlock(page);

  // Click into the text cell of the new block, type, then bold the line.
  const textCell = page.locator('.ProseMirror .studyBlocks tr[data-id] td:nth-child(2) p').first();
  await textCell.click();
  await page.keyboard.type(text);

  await page.keyboard.press('Home');
  await page.keyboard.down('Shift');
  await page.keyboard.press('End');
  await page.keyboard.up('Shift');
  await editor.clickBold();

  // Wait until bold is applied before clearing.
  await page.locator('.ProseMirror strong', { hasText: text }).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  // Select the line again and clear formatting.
  await page.keyboard.press('Home');
  await page.keyboard.down('Shift');
  await page.keyboard.press('End');
  await page.keyboard.up('Shift');
  await editor.clickClearFormatting();

  // Wait for the strong element to disappear — formatting has been cleared.
  await page.locator('.ProseMirror strong', { hasText: text }).waitFor({
    state: 'hidden',
    timeout: 10_000,
  });

  return { text };
}
