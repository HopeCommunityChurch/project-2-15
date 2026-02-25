import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { insertScripture } from '../2_molecules/insertScripture';
import { previewScriptureOnly } from '../2_molecules/previewScriptureOnly';
import { randomUUID } from 'crypto';

test.describe('editor scripture', () => {
  test('insert scripture embeds a Bible passage', async ({ page, editor, freshUser }) => {
    const title = `Editor Scripture ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await insertScripture(page, 'John 3:16');
    await editor.assertContains('For God so loved');
  });

  test('preview scripture shows passage without inserting it', async ({ page, freshUser }) => {
    const title = `Editor Preview ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    // Get the scripture block count before preview so we can confirm it didn't
    // increase (i.e. no insert occurred).
    const countBefore = await page.locator('.ProseMirror .bibleText').count();
    await previewScriptureOnly(page, 'John 3:16');
    // The preview section is visible (asserted inside previewScriptureOnly).
    // Confirm no new scripture block was inserted into the document.
    const countAfter = await page.locator('.ProseMirror .bibleText').count();
    if (countAfter !== countBefore) {
      throw new Error(
        `Expected scripture block count to stay at ${countBefore} after preview, got ${countAfter}`,
      );
    }
  });
});
