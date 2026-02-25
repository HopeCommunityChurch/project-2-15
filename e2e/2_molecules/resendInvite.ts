/**
 * resendInvite molecule — from inside an open group study modal, click the
 * "Resend" button for a pending invite identified by its share token and wait
 * for the backend to acknowledge the resend request.
 *
 * Assumes the group study modal is already open. Leaves the browser on the
 * /study/<id> page.
 */

import { Page } from '@playwright/test';
import { GroupStudyPageAtoms } from '../1_atoms/groupStudyPage';
import type { ResendInviteResult } from '../types/molecules';

export async function resendInvite(
  page: Page,
  shareToken: string,
): Promise<ResendInviteResult> {
  const groupStudy = new GroupStudyPageAtoms(page);

  const resendResponse = page.waitForResponse(
    (res) => res.url().includes('/group_study') && res.status() === 200,
    { timeout: 10_000 },
  );
  await groupStudy.clickResendInvite(shareToken);
  await resendResponse;

  return { shareToken };
}
