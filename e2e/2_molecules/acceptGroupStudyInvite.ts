/**
 * acceptGroupStudyInvite molecule — navigate to /studies, select the document
 * to use for the invite, click Accept, and wait for the browser to redirect
 * to the study editor.
 *
 * Leaves the browser on the /study/<id> editor page, now a member of the
 * group study.
 */

import { Page } from '@playwright/test';
import { GroupStudyInvitePageAtoms } from '../1_atoms/groupStudyInvitePage';
import type { AcceptGroupStudyInviteResult } from '../types/molecules';

export async function acceptGroupStudyInvite(
  page: Page,
  token: string,
  docValue: string,
): Promise<AcceptGroupStudyInviteResult> {
  const groupInvite = new GroupStudyInvitePageAtoms(page);

  await page.goto('/studies');
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  // Wait for the invite row to be rendered by HTMX before attempting to interact with it.
  await page.locator(`#share-${token}`).waitFor({ state: 'visible', timeout: 10_000 });
  await groupInvite.selectInviteDocument(token, docValue);
  await groupInvite.clickAcceptInvite(token);
  await page.waitForURL('**/study/**', { timeout: 10_000 });

  return { token, url: page.url() };
}
