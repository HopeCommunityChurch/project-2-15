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

  // Capture the count before clicking so we can wait for the (count)-th item to appear.
  const before = await page.locator('.ProseMirror .question').count();

  await editor.clickAddQuestion();

  // Wait for the newly added question node (index = before) to be visible.
  await page.locator('.ProseMirror .question').nth(before).waitFor({
    state: 'visible',
    timeout: 10_000,
  });

  return { questionIndex: before };
}
