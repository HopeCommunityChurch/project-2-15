import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { goToStudies } from '../2_molecules/goToStudies';
import { deleteStudy } from '../2_molecules/deleteStudy';
import { renameStudy } from '../2_molecules/renameStudy';
import { cancelDeleteStudy } from '../2_molecules/cancelDeleteStudy';
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

  test('rename study shows new title in the editor header', async ({ page, editor, freshUser }) => {
    const title = `Studies Rename ${randomUUID().slice(0, 8)}`;
    const newTitle = `Renamed ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await renameStudy(page, newTitle);
    // After renaming, the editor header must reflect the new title.
    await editor.assertTitle(newTitle);
  });

  test('cancel delete keeps the study in the list', async ({ page, studies, editor, freshUser }) => {
    const title = `Studies Cancel ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await cancelDeleteStudy(page);
    // Still on the editor page — study was not deleted.
    await editor.assertVisible();
    await goToStudies(page);
    await studies.assertStudyVisible(title);
  });
});
