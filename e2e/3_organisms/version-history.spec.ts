import { test, expect } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { navigateToHistory } from '../2_molecules/navigateToHistory';
import { randomUUID } from 'crypto';

test.describe('version history', () => {
  test('history page loads with "Version History" heading', async ({ page, history, freshUser }) => {
    const title = `History Load ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await navigateToHistory(page, studyId);
    await history.assertPageLoaded();
  });

  test('history page shows edit sessions after typing in a document', async ({ page, history, freshUser }) => {
    const title = `History Sessions ${randomUUID().slice(0, 8)}`;
    const typed = `session-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await page.locator('.ProseMirror[contenteditable="true"]').click();
    await page.keyboard.type(typed);
    await navigateToHistory(page, studyId);
    await history.assertHistoryGroupsLoaded();
  });

  test('clicking a history group loads a read-only preview', async ({ page, history, freshUser }) => {
    const title = `History Preview ${randomUUID().slice(0, 8)}`;
    const typed = `preview-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await page.locator('.ProseMirror[contenteditable="true"]').click();
    await page.keyboard.type(typed);
    await navigateToHistory(page, studyId);
    await history.assertHistoryGroupsLoaded();
    await history.clickFirstGroupPreviewBtn();
    await history.assertPreviewVisible();
  });

  test('"Document created" entry is visible at the bottom of the list', async ({ page, history, freshUser }) => {
    const title = `History Created ${randomUUID().slice(0, 8)}`;
    const typed = `created-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await page.locator('.ProseMirror[contenteditable="true"]').click();
    await page.keyboard.type(typed);
    await navigateToHistory(page, studyId);
    await history.assertDocumentCreatedVisible();
  });

  test('expanding an accordion reveals sub-items', async ({ page, history, freshUser }) => {
    const title = `History SubItems ${randomUUID().slice(0, 8)}`;
    const burst1 = `burst1-${randomUUID().slice(0, 8)}`;
    const burst2 = `burst2-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    const editorEl = page.locator('.ProseMirror[contenteditable="true"]');
    await editorEl.click();
    await page.keyboard.type(burst1);
    await editorEl.click();
    await page.keyboard.type(burst2);
    await navigateToHistory(page, studyId);
    await history.assertHistoryGroupsLoaded();
    await history.clickExpandButton();
    await history.assertSubItemsVisible();
  });

  test('restore flow: restoring a version lands on the study page with correct content', async ({ page, history, editor, freshUser }) => {
    const title = `History Restore ${randomUUID().slice(0, 8)}`;
    const typed = `restore-${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await editor.assertVisible();
    await editor.typeAtEnd(typed);
    await navigateToHistory(page, studyId);
    await history.assertHistoryGroupsLoaded();
    await history.clickFirstGroupPreviewBtn();
    await history.assertPreviewVisible();
    await history.assertPreviewContains(typed);
    await history.clickRestoreButton();
    await page.waitForURL(`**/study/${studyId}?restore=true`);
    await expect(page.locator('.ProseMirror[contenteditable="true"]')).toContainText(typed, { timeout: 10_000 });
  });

  test('history link in the study editor navigates to the history page', async ({ page, history, freshUser }) => {
    const title = `History Link ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await history.clickHistoryLink();
    await page.waitForURL(`**/study/${studyId}/history`);
    await history.assertPageLoaded();
  });
});
