/**
 * createGroupStudy molecule — open the Group Study modal from the study editor,
 * fill in the group name, submit the create form, and wait for the manage
 * view to load.
 *
 * Leaves the browser on the /study/<id> page with the group study manage view
 * visible inside the #groupStudy dialog.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { CreateGroupStudyResult } from '../types/molecules';

export async function createGroupStudy(
  page: Page,
  groupName: string,
): Promise<CreateGroupStudyResult> {
  const editor = new StudyPageAtoms(page);
  const groupStudy = new GroupStudyPageAtoms(page);

  await editor.clickGroupStudy();
  await groupStudy.fillCreateGroupName(groupName);
  await groupStudy.clickCreate();
  // Wait for the manage view — the group name input indicates the modal
  // has swapped to the manage state.
  await page.locator('#groupStudy input[name="groupName"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { groupName };
}
