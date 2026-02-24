// End state: browser is on /study/{studyId}/history and the history page has
// loaded (the "Version History" heading is visible).
import { Page } from '@playwright/test';
import { HistoryAtoms } from '../1_atoms/history';
import type { NavigateToHistoryResult } from '../types/molecules';

export async function navigateToHistory(
  page: Page,
  studyId: string,
): Promise<NavigateToHistoryResult> {
  const history = new HistoryAtoms(page);

  await page.goto(`/study/${studyId}/history`);
  await page.waitForURL(`**/study/${studyId}/history`);
  await history.assertPageLoaded();

  return { url: page.url(), studyId };
}
