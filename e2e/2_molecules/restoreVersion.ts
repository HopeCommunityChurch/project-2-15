// End state: browser is on /study/{studyId}?restore=true and the editor page
// has loaded. Call this after a preview has already been loaded on the history page.
import { Page } from '@playwright/test';
import { HistoryAtoms } from '../1_atoms/history';
import type { RestoreVersionResult } from '../types/molecules';

export async function restoreVersion(
  page: Page,
  studyId: string,
): Promise<RestoreVersionResult> {
  const history = new HistoryAtoms(page);

  await history.clickRestoreButton();
  await page.waitForURL(`**/study/${studyId}?restore=true`, { timeout: 10_000 });

  return { studyId, url: page.url() };
}
