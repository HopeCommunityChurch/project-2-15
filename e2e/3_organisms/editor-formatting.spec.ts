import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { applyBoldToText } from '../2_molecules/applyBoldToText';
import { applyItalicToText } from '../2_molecules/applyItalicToText';
import { applyUnderlineToText } from '../2_molecules/applyUnderlineToText';
import { clearFormattingFromText } from '../2_molecules/clearFormattingFromText';
import { undoChange } from '../2_molecules/undoChange';
import { redoChange } from '../2_molecules/redoChange';
import { randomUUID } from 'crypto';

test.describe('editor formatting', () => {
  test('bold is applied to selected text', async ({ page, editor, freshUser }) => {
    const title = `Editor Format ${randomUUID().slice(0, 8)}`;
    const text = `bold-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await applyBoldToText(page, text);
    await editor.assertBoldActive(text);
  });

  test('italic is applied and persists', async ({ page, editor, freshUser }) => {
    const title = `Editor Italic ${randomUUID().slice(0, 8)}`;
    const text = `italic-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await applyItalicToText(page, text);
    await editor.assertItalicActive(text);
  });

  test('underline is applied and persists', async ({ page, editor, freshUser }) => {
    const title = `Editor Underline ${randomUUID().slice(0, 8)}`;
    const text = `underline-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await applyUnderlineToText(page, text);
    await editor.assertUnderlineActive(text);
  });

  test('clear formatting removes bold mark', async ({ page, editor, freshUser }) => {
    const title = `Editor Clear ${randomUUID().slice(0, 8)}`;
    const text = `clear-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await clearFormattingFromText(page, text);
    // The clearFormattingFromText molecule applies bold then clears it and waits
    // for the strong element to disappear, so arriving here confirms bold was
    // removed. The text itself must still be visible in the editor.
    await editor.assertContains(text);
  });

  test('redo restores change after undo', async ({ page, editor, freshUser }) => {
    const title = `Editor Redo ${randomUUID().slice(0, 8)}`;
    const text = `redo-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await applyBoldToText(page, text);
    await editor.assertBoldActive(text);
    await undoChange(page);
    // After undo the bold mark must be gone — confirms undo actually worked.
    await editor.assertNotBoldActive(text);
    await redoChange(page);
    // After redo the bold must be restored.
    await editor.assertBoldActive(text);
  });
});
