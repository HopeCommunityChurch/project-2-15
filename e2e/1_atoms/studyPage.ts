import { expect, Page } from '@playwright/test';
import type { IStudyPageAtoms } from '../types/atoms';

export class StudyPageAtoms implements IStudyPageAtoms {
  constructor(private page: Page) {}

  /** The ProseMirror contenteditable element. */
  private editor() {
    return this.page.locator('.ProseMirror[contenteditable="true"]');
  }

  /** Asserts the editor is present and interactive. */
  async assertVisible() {
    await expect(this.editor()).toBeVisible();
  }

  /** Types text into the editor body (below the section heading). */
  async typeAtEnd(text: string) {
    // Click the paragraph below the heading (shown as placeholder emphasis text when empty).
    await this.editor().locator('p, em').first().click();
    await this.page.keyboard.type(text);
  }

  /** Asserts the editor contains the given text somewhere in its content. */
  async assertContains(text: string) {
    await expect(this.editor()).toContainText(text);
  }

  /**
   * Asserts the editor contains the given text within a bounded timeout.
   * Use this for real-time sync assertions to encode the WebSocket SLA.
   */
  async assertContainsWithTimeout(text: string, ms: number) {
    await expect(this.editor()).toContainText(text, { timeout: ms });
  }

  /** Asserts the save-state indicator shows "saved" (paragraph text in the header). */
  async assertSaved() {
    await expect(this.page.locator('header p', { hasText: 'saved' })).toBeVisible();
  }

  /** Asserts the study title shown in the editor header. */
  async assertTitle(title: string) {
    await expect(this.page.locator('header p', { hasText: title })).toBeVisible();
  }
}
