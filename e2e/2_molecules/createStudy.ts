/**
 * createStudy molecule — click "+ New Study", fill title, submit, assert
 * the app navigates to the new study's editor page.
 *
 * Leaves the browser on the /study/<id> editor page.
 */

import { Page } from '@playwright/test';
import { StudiesPageAtoms } from '../1_atoms/studiesPage';
import type { CreateStudyResult } from '../types/molecules';

export async function createStudy(
  page: Page,
  title: string,
): Promise<CreateStudyResult> {
  const studies = new StudiesPageAtoms(page);

  await studies.clickAddStudy();
  await studies.fillNewStudyTitle(title);
  await studies.clickCreateStudy();
  // First load of a new study; backend creates the document before redirecting.
  await page.waitForURL('**/study/**', { timeout: 10_000 });

  const url = page.url();
  const match = url.match(/\/study\/([^/]+)/);
  if (!match) throw new Error(`createStudy: could not extract studyId from URL: ${url}`);
  const studyId = match[1];
  return { title, url, studyId };
}
