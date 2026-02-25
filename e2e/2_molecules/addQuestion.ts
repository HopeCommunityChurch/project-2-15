/**
 * addQuestion molecule — click the "Add Question" toolbar button and wait
 * for the question node to appear in the ProseMirror editor.
 *
 * Leaves the browser on the /study/<id> page with a new question inserted.
 */

import { Page } from '@playwright/test';
import { StudyPageAtoms } from '../1_atoms/studyPage';
import type { AddQuestionResult } from '../types/molecules';

export async function addQuestion(page: Page): Promise<AddQuestionResult> {
  const editor = new StudyPageAtoms(page);

  // Capture the count of existing question nodes before clicking.
  // Individual questions are <questionouter> elements inside the .questions row.
  const before = await page.locator('.ProseMirror questionouter').count();

  await editor.clickAddQuestion();

  // Wait for the newly added question node (index = before) to be visible.
  await page.locator('.ProseMirror questionouter').nth(before).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { questionIndex: before };
}
