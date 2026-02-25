/**
 * rejectGroupStudyInvite molecule — navigate to /studies, click the Reject
 * button for the given invite token, and wait for the invite row to be removed
 * from the DOM.
 *
 * Leaves the browser on the /studies page with the invite gone.
 */

import { Page } from '@playwright/test';
import { GroupStudyInvitePageAtoms } from '../1_atoms/groupStudyInvitePage';
import type { RejectGroupStudyInviteResult } from '../types/molecules';

export async function rejectGroupStudyInvite(
  page: Page,
  token: string,
): Promise<RejectGroupStudyInviteResult> {
  const groupInvite = new GroupStudyInvitePageAtoms(page);

  await page.goto('/studies');
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  // Wait for the invite row to be rendered by HTMX before attempting to click it.
  await page.locator(`#share-${token}`).waitFor({ state: 'visible', timeout: 10_000 });
  await groupInvite.clickRejectInvite(token);
  await page.locator(`#share-${token}`).waitFor({ state: 'detached', timeout: 10_000 });

  return { token };
}
