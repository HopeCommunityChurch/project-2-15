/**
 * removeMember molecule — open the Group Study modal and click the Remove
 * button for a member identified by their document ID, then wait for the
 * member row to detach from the DOM.
 *
 * Leaves the browser on the /study/<id> page with the member row gone.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { RemoveMemberResult } from '../types/molecules';

export async function removeMember(
  page: Page,
  docId: string,
): Promise<RemoveMemberResult> {
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
  await groupStudy.clickRemoveMember(docId);
  await page.locator(`#member-doc-${docId}`).waitFor({
    state: 'detached',
    timeout: 10_000,
  });

  return { docId };
}
