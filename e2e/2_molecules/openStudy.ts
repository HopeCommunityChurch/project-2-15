/**
 * openStudy molecule — navigate directly to an existing study editor by its
 * studyId and wait for the ProseMirror editor to become interactive.
 *
 * Use this to connect a second browser context (tab/user) to a study that was
 * already created by a primary context. Leaves the browser on the
 * /study/<studyId> editor page with the editor visible.
 */

import { Page } from '@playwright/test';
import type { OpenStudyResult } from '../types/molecules';

export async function openStudy(
  page: Page,
  studyId: string,
): Promise<OpenStudyResult> {
  await page.goto(`/study/${studyId}`);
  await page.waitForURL(`**/study/${studyId}`, { timeout: 10_000 });
  // Wait for the ProseMirror editor to be interactive — confirms DocOpened
  // WS handshake has completed and the editor is ready for input.
  await page.locator('.ProseMirror[contenteditable="true"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { url: page.url(), studyId };
}
