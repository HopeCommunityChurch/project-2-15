import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { goToStudies } from '../2_molecules/goToStudies';
import { deleteStudy } from '../2_molecules/deleteStudy';
import { randomUUID } from 'crypto';

test.describe('studies', () => {
  test('create study navigates to the editor @smoke', async ({ page, editor, freshUser }) => {
    const title = `Studies Smoke ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();
    await editor.assertTitle(title);
  });

  test('created study appears in the studies list', async ({ page, studies, freshUser }) => {
    const title = `Studies List ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await goToStudies(page);
    await studies.assertStudyVisible(title);
  });

  test('delete study removes it from the list', async ({ page, studies, freshUser }) => {
    const title = `Studies Delete ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await deleteStudy(page);
    await studies.assertStudyNotVisible(title);
  });
});
