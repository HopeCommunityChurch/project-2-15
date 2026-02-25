import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { addSection } from '../2_molecules/addSection';
import { addStudyBlock } from '../2_molecules/addStudyBlock';
import { addQuestion } from '../2_molecules/addQuestion';
import { randomUUID } from 'crypto';

test.describe('editor structure', () => {
  test('add section inserts a sidebar entry', async ({ page, freshUser }) => {
    const title = `Editor Section ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await addSection(page);
  });

  test('add study block inserts a block node', async ({ page, freshUser }) => {
    const title = `Editor Block ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await addStudyBlock(page);
  });

  test('add question inserts a question node', async ({ page, freshUser }) => {
    const title = `Editor Question ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await addQuestion(page);
  });
});
