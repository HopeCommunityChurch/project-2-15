/**
 * renameStudy molecule — click the study title contenteditable in the editor
 * header, clear the current value, type the new title, and trigger a save via
 * Ctrl+S, waiting for the "saved" indicator to confirm the change persisted.
 *
 * Leaves the browser on the /study/<id> editor page with the new title saved.
 */

import { Page } from '@playwright/test';
import type { RenameStudyResult } from '../types/molecules';

export async function renameStudy(
  page: Page,
  title: string,
): Promise<RenameStudyResult> {
  // The study title is a contenteditable element with id="studyName".
  const titleEl = page.locator('#studyName');
  await titleEl.waitFor({ state: 'visible', timeout: 10_000 });

  // Triple-click to select all existing text in the contenteditable, then type
  // the new title to replace it.
  await titleEl.click({ clickCount: 3 });
  await page.keyboard.type(title);

  // Trigger an explicit save so we don't rely on the debounce settling.
  await page.keyboard.press('Control+s');

  // Wait for the "saved" indicator to confirm persistence.
  await page.locator('header p', { hasText: 'saved' }).waitFor({
    state: 'visible',
    timeout: 15_000,
  });

  return { title };
}
