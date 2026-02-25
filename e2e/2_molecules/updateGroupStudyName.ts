/**
 * updateGroupStudyName molecule — inside an open group study modal, fill the
 * group name input with a new value and wait for the "Saved!" auto-save
 * notification to confirm the change has been persisted.
 *
 * Assumes the group study modal is already open. Leaves the browser on the
 * /study/<id> page with the group name updated.
 */

import { Page } from '@playwright/test';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { UpdateGroupStudyNameResult } from '../types/molecules';

export async function updateGroupStudyName(
  page: Page,
  groupName: string,
): Promise<UpdateGroupStudyNameResult> {
  const groupStudy = new GroupStudyPageAtoms(page);

  await groupStudy.fillGroupName(groupName);
  // The name auto-saves after a 1-second debounce; wait for the "Saved!" text.
  await groupStudy.assertGroupNameSaved();

  return { groupName };
}
