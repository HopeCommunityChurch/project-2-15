/**
 * updateGroupStudyName molecule — inside an open group study modal, fill the
 * group name input with a new value and wait for the HTMX auto-save request
 * to complete.
 *
 * Assumes the group study modal is already open with the manage view visible.
 * Leaves the browser on the /study/<id> page with the group name updated.
 */

import { Page } from '@playwright/test';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { UpdateGroupStudyNameResult } from '../types/molecules';

export async function updateGroupStudyName(
  page: Page,
  groupName: string,
): Promise<UpdateGroupStudyNameResult> {
  const groupStudy = new GroupStudyPageAtoms(page);

  // Set up the save response wait before typing so we don't miss it.
  // The name input posts to /group_study/<id>/name on keyup debounce.
  const saveResponse = page.waitForResponse(
    (res) => res.url().includes('/group_study/') && res.url().includes('/name'),
    { timeout: 10_000 },
  );

  await groupStudy.fillGroupName(groupName);

  // The name auto-saves after a 1-second debounce (hx-trigger="keyup changed delay:1s").
  await saveResponse;

  return { groupName };
}
