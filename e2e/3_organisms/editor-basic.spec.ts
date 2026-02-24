import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { TEST_ACCOUNTS } from '../fixtures/credentials';
import { randomUUID } from 'crypto';

const { email: EMAIL, password: PASSWORD } = TEST_ACCOUNTS.main;

test.describe('editor basic', () => {
  test('editor loads after creating a study', async ({ page, editor }) => {
    const title = `Editor Basic Test ${randomUUID().slice(0, 8)}`;
    await login(page, EMAIL, PASSWORD);
    await createStudy(page, title);
    await editor.assertVisible();
    await editor.assertTitle(title);
  });

  test('typed text appears in the editor', async ({ page, editor }) => {
    const title = `Editor Type Test ${randomUUID().slice(0, 8)}`;
    const typedText = `hello-${randomUUID().slice(0, 8)}`;
    await login(page, EMAIL, PASSWORD);
    await createStudy(page, title);
    await editor.typeAtEnd(typedText);
    await editor.assertContains(typedText);
  });
});
