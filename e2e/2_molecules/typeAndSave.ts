/**
 * typeAndSave molecule — type text into the study editor, trigger an explicit
 * save with Ctrl+S, and wait for the "saved" header indicator to confirm the
 * document has been persisted.
 *
 * Leaves the browser on the /study/<id> editor page with the text saved.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { TypeAndSaveResult } from '../types/molecules';

export async function typeAndSave(
  page: Page,
  text: string,
): Promise<TypeAndSaveResult> {
  const editor = new StudyPageAtoms(page);

  await editor.typeAtEnd(text);

  // If a prior save left the indicator visible, wait for it to disappear first
  // so the subsequent appearance confirms THIS save was acknowledged — not the
  // previous one.
  await page.locator('header p', { hasText: 'saved' }).waitFor({
    state: 'hidden',
    timeout: 5_000,
  }).catch(() => {
    // Indicator was already hidden (common case) — proceed.
  });

  // Trigger explicit save via Ctrl+S so we don't have to wait for the debounce.
  await page.keyboard.press('Control+s');

  // Wait for the "saved" indicator in the header — the app sets this text once
  // the backend acknowledges the save via the WebSocket SaveDoc confirmation.
  await page.locator('header p', { hasText: 'saved' }).waitFor({
    state: 'visible',
    timeout: 15_000,
  });

  return { text };
}
