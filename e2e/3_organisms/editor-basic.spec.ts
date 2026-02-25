import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { randomUUID } from 'crypto';

test.describe('editor basic', () => {
  test('editor loads after creating a study @smoke', async ({ page, editor, freshUser }) => {
    const title = `Editor Basic Test ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();
    await editor.assertTitle(title);
  });

  test('typed text appears in the editor', async ({ page, editor, freshUser }) => {
    const title = `Editor Type Test ${randomUUID().slice(0, 8)}`;
    const typedText = `hello-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.typeAtEnd(typedText);
    await editor.assertContains(typedText);
  });
});
