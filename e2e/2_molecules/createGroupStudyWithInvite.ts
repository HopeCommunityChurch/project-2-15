/**
 * createGroupStudyWithInvite molecule — open the Group Study modal from the
 * study editor, fill in the group name plus an initial invite (email and
 * permission), then submit the create form and wait for the manage view.
 *
 * Leaves the browser on the /study/<id> page with the group study manage view
 * visible and the invite already queued.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { CreateGroupStudyWithInviteResult } from '../types/molecules';

export async function createGroupStudyWithInvite(
  page: Page,
  groupName: string,
  inviteEmail: string,
  permission: 'member' | 'owner',
): Promise<CreateGroupStudyWithInviteResult> {
  const editor = new StudyPageAtoms(page);
  const groupStudy = new GroupStudyPageAtoms(page);

  await editor.clickGroupStudy();
  await groupStudy.fillCreateGroupName(groupName);
  await groupStudy.fillCreateInviteEmail(inviteEmail);
  await groupStudy.selectCreateInvitePermission(permission);
  await groupStudy.clickCreate();
  // Wait for the manage view.
  await page.locator('#groupStudy input[name="groupName"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { groupName, inviteEmail, permission };
}
