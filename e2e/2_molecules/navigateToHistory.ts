// End state: browser is on /study/{studyId}/history and the history page has
// loaded (the "Version History" heading is visible).
import { Page } from '@playwright/test';
import type { NavigateToHistoryResult } from '../types/molecules';

export async function navigateToHistory(
  page: Page,
  studyId: string,
): Promise<NavigateToHistoryResult> {
  await page.goto(`/study/${studyId}/history`);
  await page.waitForURL(`**/study/${studyId}/history`);

  return { url: page.url(), studyId };
}
