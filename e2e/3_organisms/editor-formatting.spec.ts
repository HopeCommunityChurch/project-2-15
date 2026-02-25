import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { applyBoldToText } from '../2_molecules/applyBoldToText';
import { randomUUID } from 'crypto';

test.describe('editor formatting', () => {
  test('bold is applied to selected text', async ({ page, editor, freshUser }) => {
    const title = `Editor Format ${randomUUID().slice(0, 8)}`;
    const text = `bold-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await applyBoldToText(page, text);
    await editor.assertBoldActive();
  });
});
