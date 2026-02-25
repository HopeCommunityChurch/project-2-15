import { expect, Page } from '@playwright/test';
import type { IHistoryPageAtoms } from '../types/atoms';

export class HistoryPageAtoms implements IHistoryPageAtoms {
  constructor(private page: Page) {}

  // ── Session list (left sidebar) ──────────────────────────────────────────────

  /**
   * Clicks a history session row by zero-based index to select that version.
   * Selector: [data-testid="history-session"] (nth index)
   */
  async clickSession(index: number) {
    await this.page.locator('[data-testid="history-session"]').nth(index).click();
  }

  /**
   * Clicks a history session row whose displayed label contains the given text
   * (e.g. a date string or version number). Useful when the exact index is
   * unknown. Selector: [data-testid="history-session"]:has-text(text)
   */
  async clickSessionByText(text: string) {
    await this.page
      .locator('[data-testid="history-session"]', { hasText: text })
      .click();
  }

  // ── Preview editor (read-only ProseMirror) ───────────────────────────────────

  /**
   * Asserts the read-only preview editor contains the given text.
   * This verifies that the document was correctly reconstructed at the selected
   * version. Selector: #historyPreview .ProseMirror
   */
  async assertPreviewContains(text: string) {
    await expect(
      this.page.locator('#historyPreview .ProseMirror')
    ).toContainText(text);
  }

  /**
   * Asserts the read-only preview editor does NOT contain the given text.
   * Use to verify that edits made after a version are absent in that snapshot.
   * Selector: #historyPreview .ProseMirror
   */
  async assertPreviewNotContains(text: string) {
    await expect(
      this.page.locator('#historyPreview .ProseMirror')
    ).not.toContainText(text);
  }

  /**
   * Asserts the preview editor is visible (i.e. the history page has fully
   * loaded and rendered the reconstructed document).
   * Selector: #historyPreview .ProseMirror
   */
  async assertPreviewVisible() {
    await expect(
      this.page.locator('#historyPreview .ProseMirror')
    ).toBeVisible();
  }

  // ── Version / session metadata ───────────────────────────────────────────────

  /**
   * Asserts that the active (selected) session row shows the expected version
   * label — e.g. "v42" or "Version 42". Verifies the UI updates when a
   * different session is selected. Selector: [data-testid="history-session"].active
   * or [aria-current="true"]
   */
  async assertActiveSessionVersion(label: string) {
    await expect(
      this.page.locator('[data-testid="history-session"][aria-current="true"]')
    ).toContainText(label);
  }

  /**
   * Asserts a specific number of session rows are present in the sidebar.
   * Verifies that the backend grouped steps into the expected number of sessions.
   * Selector: [data-testid="history-session"]
   */
  async assertSessionCount(count: number) {
    await expect(
      this.page.locator('[data-testid="history-session"]')
    ).toHaveCount(count);
  }

  // ── Navigation ───────────────────────────────────────────────────────────────

  /**
   * Clicks the back link that returns to the study/editor page.
   * Selector: a[data-testid="history-back"] or link with text "Back"
   */
  async clickBack() {
    await this.page.locator('[data-testid="history-back"]').click();
  }
}
