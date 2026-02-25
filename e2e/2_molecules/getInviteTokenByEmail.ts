/**
 * getInviteTokenByEmail molecule — navigate to the study editor as the group
 * study owner, open the Group Study manage modal, and find the share token for
 * the pending invite sent to the given email address.
 *
 * The owner's manage view renders each pending invite as a
 * `.share.person-row[id="share-<token>"]` element whose `.person-name` contains
 * the invitee's email.  This is the only place in the UI where the token and
 * email are co-located.
 *
 * Leaves the browser on the /study/<id> page with the modal open.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { GetInviteTokenByEmailResult } from '../types/molecules';

export async function getInviteTokenByEmail(
  page: Page,
  studyId: string,
  email: string,
): Promise<GetInviteTokenByEmailResult> {
  const editor = new StudyPageAtoms(page);

  await page.goto(`/study/${studyId}`);
  await page.waitForURL(`**/study/${studyId}**`, { timeout: 10_000 });

  // Ensure the modal is closed before opening it (avoids backdrop issues).
  await page.locator('#groupStudy').evaluate((el) => {
    if (typeof (el as any).close === 'function' && (el as any).open) (el as any).close();
  }).catch(() => { /* dialog not in DOM yet — safe to proceed */ });

  await editor.clickGroupStudy();
  // Wait for the manage view to load (HTMX async swap).
  await page.locator('#groupStudy input[name="groupName"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  // The invite row has id="share-<token>" and its .person-name contains the email.
  const row = page.locator('#groupStudy .share.person-row', { hasText: email });
  await row.waitFor({ state: 'visible', timeout: 10_000 });
  const id = await row.getAttribute('id', { timeout: 10_000 });
  if (!id) throw new Error(`getInviteTokenByEmail: no row id found for email: ${email}`);
  const token = id.replace(/^share-/, '');
  if (!token) throw new Error(`getInviteTokenByEmail: could not parse token from id: ${id}`);

  return { token };
}
