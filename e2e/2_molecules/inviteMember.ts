/**
 * inviteMember molecule — open the Group Study modal, fill in the invite email
 * and permission, submit, and wait for the invite row to appear in the
 * "People with access" list.
 *
 * Assumes the group study manage view is already reachable (i.e. the study
 * already has a group study). Leaves the browser on the /study/<id> page with
 * the invite row visible in the #groupStudy dialog.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { InviteMemberResult } from '../types/molecules';

export async function inviteMember(
  page: Page,
  email: string,
  permission: 'member' | 'owner',
): Promise<InviteMemberResult> {
  const editor = new StudyPageAtoms(page);
  const groupStudy = new GroupStudyPageAtoms(page);

  // Close the modal properly (showModal creates a backdrop that only dialog.close() removes).
  await page.locator('#groupStudy').evaluate((el) => {
    if (typeof (el as any).close === 'function' && (el as any).open) (el as any).close();
  }).catch(() => { /* dialog not in DOM yet — safe to proceed */ });
  await editor.clickGroupStudy();
  // Wait for the manage view to load inside the modal (HTMX async swap).
  await page.locator('#groupStudy input[name="groupName"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });
  await groupStudy.fillInviteEmail(email);
  await groupStudy.selectInvitePermission(permission);
  await groupStudy.clickInvite();
  // Wait for the invite row to appear.
  await page.locator('#groupStudy .share .person-name', { hasText: email }).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { email, permission };
}
