import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { typeAndSave } from '../2_molecules/typeAndSave';
import { insertScripture } from '../2_molecules/insertScripture';
import { undoChange } from '../2_molecules/undoChange';
import { deleteSection } from '../2_molecules/deleteSection';
import { randomUUID } from 'crypto';

test.describe('document persistence', () => {
  test('typed text persists after a full page reload @smoke', async ({ page, editor, freshUser }) => {
    const title = `Persist Reload ${randomUUID().slice(0, 8)}`;
    const text = `persist-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await typeAndSave(page, text);
    await editor.assertContains(text);

    // Reload the page and verify the text is still present.
    await page.reload();
    await page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 15_000 });
    await editor.assertContains(text);
  });

  test('undo after inserting a scripture block removes the block', async ({ page, editor, freshUser }) => {
    const title = `Persist Undo ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    const before = await page.locator('.ProseMirror .bibleText').count();
    await insertScripture(page, 'John 3:16');
    // Confirm scripture was inserted.
    await editor.assertContains('For God so loved');
    await undoChange(page);
    // The bibleText node count should return to its prior value.
    await page.locator('.ProseMirror .bibleText').nth(before).waitFor({ state: 'detached', timeout: 10_000 }).catch(() => {
      // If count was 0 before, nth(0) won't detach — just verify total count.
    });
    const after = await page.locator('.ProseMirror .bibleText').count();
    if (after > before) throw new Error(`Expected bibleText count to return to ${before} after undo, got ${after}`);
  });

  test('delete section removes it from both sidebar and editor', async ({ page, editor, freshUser }) => {
    const title = `Persist Del Section ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);

    // Wait for the initial section to be visible before counting.
    await page.locator('#leftSidebar .section').first().waitFor({ state: 'visible', timeout: 10_000 });
    const countBefore = await page.locator('#leftSidebar .section').count();

    await deleteSection(page, 0);
    // After deletion the count must be less than before.
    await editor.assertSidebarSectionCount(countBefore - 1);
  });

  test('indent on a scripture chunk increases the visual indentation level', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Persist Indent ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await insertScripture(page, 'John 3:16');

    // Click into the scripture chunk to place caret there, then indent.
    await page.locator('.ProseMirror .bibleText .chunk').first().click();
    await editor.clickIndent();

    // The first bibleText block's first chunk should now have level >= 1.
    await editor.assertScriptureChunkIndentLevel(0, 1);
  });
});
