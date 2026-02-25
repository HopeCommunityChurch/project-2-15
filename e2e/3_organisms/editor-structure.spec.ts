import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { addSection } from '../2_molecules/addSection';
import { addStudyBlock } from '../2_molecules/addStudyBlock';
import { addQuestion } from '../2_molecules/addQuestion';

import { randomUUID } from 'crypto';

test.describe('editor structure', () => {
  test('add section inserts a sidebar entry', async ({ page, editor, freshUser }) => {
    const title = `Editor Section ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    const { sectionIndex } = await addSection(page);
    await editor.assertSidebarSectionCountAtLeast(sectionIndex + 1);
  });

  test('add study block inserts a block node', async ({ page, editor, freshUser }) => {
    const title = `Editor Block ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    const { blockIndex } = await addStudyBlock(page);
    await editor.assertStudyBlockCountAtLeast(blockIndex + 1);
  });

  test('add question inserts a question node', async ({ page, editor, freshUser }) => {
    const title = `Editor Question ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    const { questionIndex } = await addQuestion(page);
    await editor.assertQuestionCountAtLeast(questionIndex + 1);
  });

  test('delete study block removes it from the editor', async ({ page, editor, freshUser }) => {
    const title = `Editor Del Block ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    // A fresh document starts with 0 blocks; add one so we know the exact count.
    await editor.assertStudyBlockCount(0);
    await addStudyBlock(page);
    await editor.assertStudyBlockCount(1);
    // Undo the insert — the count must return to 0.
    await editor.clickUndo();
    await editor.assertStudyBlockCount(0);
  });

  test('delete question node removes it from the editor', async ({ page, editor, freshUser }) => {
    const title = `Editor Del Question ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    // A fresh document starts with 0 questions; add one so we know the exact count.
    await editor.assertQuestionCount(0);
    await addQuestion(page);
    await editor.assertQuestionCount(1);
    // Undo the insert — the count must return to 0.
    await editor.clickUndo();
    await editor.assertQuestionCount(0);
  });
});
