/**
 * acceptGroupStudyInviteWithExistingDoc molecule — navigate to /studies,
 * select an existing document by its docId for the invite, click Accept, and
 * wait for the browser to redirect to the study editor.
 *
 * Use this variant when you want to merge the group study into a document the
 * user already owns, rather than creating a new document. The `docId` must be
 * the UUID of the existing document.
 *
 * Leaves the browser on the /study/<id> editor page, now a member of the group
 * study, using the pre-existing document.
 */

import { Page } from '@playwright/test';
import { GroupStudyInvitePageAtoms } from '../1_atoms/groupStudyInvitePage';
import type { AcceptGroupStudyInviteWithExistingDocResult } from '../types/molecules';

export async function acceptGroupStudyInviteWithExistingDoc(
  page: Page,
  token: string,
  docId: string,
): Promise<AcceptGroupStudyInviteWithExistingDocResult> {
  const groupInvite = new GroupStudyInvitePageAtoms(page);

  await page.goto('/studies');
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  // Wait for the invite row to be rendered by HTMX before attempting to interact.
  await page.locator(`#share-${token}`).waitFor({ state: 'visible', timeout: 10_000 });
  // Pass the existing docId as the p-select value (the option value matches the docId).
  await groupInvite.selectInviteDocument(token, docId);
  await groupInvite.clickAcceptInvite(token);
  await page.waitForURL('**/study/**', { timeout: 10_000 });

  return { token, url: page.url() };
}
