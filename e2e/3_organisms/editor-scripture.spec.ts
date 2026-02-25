import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { insertScripture } from '../2_molecules/insertScripture';
import { randomUUID } from 'crypto';

test.describe('editor scripture', () => {
  test('insert scripture embeds a Bible passage', async ({ page, editor, freshUser }) => {
    const title = `Editor Scripture ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await insertScripture(page, 'John 3:16');
    await editor.assertContains('For God so loved');
  });
});
