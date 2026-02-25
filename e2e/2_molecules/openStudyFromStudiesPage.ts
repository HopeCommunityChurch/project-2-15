/**
 * openStudyFromStudiesPage molecule — navigate to /studies, click a study card
 * by its title, and wait for the ProseMirror editor to become interactive.
 *
 * Leaves the browser on the /study/<id> editor page with the editor visible.
 */

import { Page } from '@playwright/test';
import { StudiesPageAtoms } from '../1_atoms/studiesPage';
import type { OpenStudyFromStudiesPageResult } from '../types/molecules';

export async function openStudyFromStudiesPage(
  page: Page,
  title: string,
): Promise<OpenStudyFromStudiesPageResult> {
  const studies = new StudiesPageAtoms(page);

  await page.goto('/studies');
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  await studies.clickStudy(title);
  await page.waitForURL('**/study/**', { timeout: 10_000 });

  // Wait for the ProseMirror editor to be interactive — confirms the WS
  // handshake has completed and the editor is ready for input.
  await page.locator('.ProseMirror[contenteditable="true"]').waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  const url = page.url();
  const match = url.match(/\/study\/([^/]+)/);
  if (!match) throw new Error(`openStudyFromStudiesPage: could not extract studyId from URL: ${url}`);
  const studyId = match[1];

  return { url, studyId };
}
