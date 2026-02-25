/**
 * changeMemberRole molecule — open the Group Study modal, change the ownership
 * role for a member identified by their document ID, and wait for the save
 * response to confirm the change was persisted.
 *
 * Leaves the browser on the /study/<id> page with the member role updated.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { ChangeMemberRoleResult } from '../types/molecules';

export async function changeMemberRole(
  page: Page,
  docId: string,
  role: 'member' | 'owner',
): Promise<ChangeMemberRoleResult> {
  const editor = new StudyPageAtoms(page);
  const groupStudy = new GroupStudyPageAtoms(page);

  await editor.clickGroupStudy();
  // Wait for the manage view to load inside the modal (HTMX async swap).
  await page.locator('#groupStudy input[name="groupName"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  const saveResponse = page.waitForResponse(
    (res) =>
      res.url().includes('/groupstudy') &&
      res.request().method() !== 'GET' &&
      res.status() === 200,
    { timeout: 10_000 },
  );
  await groupStudy.selectMemberOwnership(docId, role);
  await saveResponse;

  return { docId, role };
}
