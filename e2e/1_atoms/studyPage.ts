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

  // ── Toolbar: formatting ────────────────────────────────────────────────────

  /** Clicks the Bold toolbar button. Selector: button[title^="Bold"] in #editorToolbar */
  async clickBold() {
    await this.page.locator('#editorToolbar button[title^="Bold"]').click();
  }

  /** Clicks the Italic toolbar button. Selector: button[title^="Italic"] in #editorToolbar */
  async clickItalic() {
    await this.page.locator('#editorToolbar button[title^="Italic"]').click();
  }

  /** Clicks the Underline toolbar button. Selector: button[title^="Underline"] in #editorToolbar */
  async clickUnderline() {
    await this.page.locator('#editorToolbar button[title^="Underline"]').click();
  }

  /** Clicks the Undo toolbar button. Selector: button[title="Undo"] in #editorToolbar */
  async clickUndo() {
    await this.page.locator('#editorToolbar button[title="Undo"]').click();
  }

  /** Clicks the Redo toolbar button. Selector: button[title="Redo"] in #editorToolbar */
  async clickRedo() {
    await this.page.locator('#editorToolbar button[title="Redo"]').click();
  }

  /** Clicks the Outdent toolbar button. Selector: button[title^="Outdent"] in #editorToolbar */
  async clickOutdent() {
    await this.page.locator('#editorToolbar button[title^="Outdent"]').click();
  }

  /** Clicks the Indent toolbar button. Selector: button[title^="Indent"] in #editorToolbar */
  async clickIndent() {
    await this.page.locator('#editorToolbar button[title^="Indent"]').click();
  }

  /** Clicks the Clear Formatting toolbar button. Selector: button[title="Clear Formatting"] in #editorToolbar */
  async clickClearFormatting() {
    await this.page.locator('#editorToolbar button[title="Clear Formatting"]').click();
  }

  /** Asserts that selected text is bold (strong element wraps the text). */
  async assertBoldActive() {
    await expect(this.editor().locator('strong')).toBeVisible();
  }

  /** Asserts that selected text is italic (em element wraps the text). */
  async assertItalicActive() {
    await expect(this.editor().locator('em')).toBeVisible();
  }

  /** Asserts that selected text is underlined (u element wraps the text). */
  async assertUnderlineActive() {
    await expect(this.editor().locator('u')).toBeVisible();
  }

  // ── Toolbar: Bible search (Insert Scripture) ───────────────────────────────

  /** Clicks the Insert Scripture toolbar button to open the scripture search popup. */
  async clickInsertScripture() {
    await this.page.locator('#editorToolbar button[title="Insert Scripture"]').click();
  }

  /** Types a Bible reference into the scripture search input. Selector: #addScripture */
  async fillScriptureRef(ref: string) {
    await this.page.locator('#addScripture').fill(ref);
  }

  /** Clicks the Preview button in the scripture popup to fetch the passage. */
  async clickScripturePreview() {
    await this.page.locator('#addScripturePopup button[type="submit"]:not(#addScriptureButton)').click();
  }

  /** Clicks the Insert button in the scripture popup to insert the previewed passage. */
  async clickScriptureInsert() {
    await this.page.locator('#addScriptureButton').click();
  }

  /** Asserts the scripture preview section is visible (after clicking Preview). */
  async assertScripturePreviewVisible() {
    await expect(this.page.locator('#previewScripture')).toBeVisible();
  }

  // ── Toolbar: study structure ───────────────────────────────────────────────

  /** Clicks the Add Study Block toolbar button. Selector: button[title="Add Study Block"] */
  async clickAddStudyBlock() {
    await this.page.locator('#editorToolbar button[title="Add Study Block"]').click();
  }

  /** Clicks the Add Question toolbar button. Selector: button[title^="Add Question"] */
  async clickAddQuestion() {
    await this.page.locator('#editorToolbar button[title^="Add Question"]').click();
  }

  // ── Left sidebar ───────────────────────────────────────────────────────────

  /** Clicks the Add Section button in the left sidebar. Selector: #leftSidebar button "Add Section" */
  async clickAddSection() {
    await this.page.locator('#leftSidebar button', { hasText: 'Add Section' }).click();
  }

  /** Clicks the collapse/expand sidebar toggle button. Selector: #collapseSide */
  async clickCollapseSidebar() {
    await this.page.locator('#collapseSide').click();
  }

  // ── Profile nav / header actions ──────────────────────────────────────────

  /** Opens the profile nav dialog by clicking the user initials button in the header. */
  async clickProfileMenu() {
    await this.page.locator('header .profileButton').click();
  }

  /** Clicks the "Delete Study" link inside the profile nav dialog to open the confirm-delete dialog. */
  async clickDeleteStudy() {
    await this.page.locator('#profileNav a', { hasText: 'Delete Study' }).click();
  }

  // ── Delete confirmation dialog ────────────────────────────────────────────

  /** Clicks the "Yes, delete" button in the confirm-delete dialog. Selector: #confirmDelete button.red */
  async clickConfirmDelete() {
    await this.page.locator('#confirmDelete button.red').click();
  }

  /** Clicks the Cancel button in the confirm-delete dialog. Selector: #confirmDelete button.lightBlue */
  async clickCancelDelete() {
    await this.page.locator('#confirmDelete button.lightBlue').click();
  }

  /** Asserts the confirm-delete dialog is visible. */
  async assertConfirmDeleteVisible() {
    await expect(this.page.locator('#confirmDelete')).toBeVisible();
  }

  // ── Group Study button ────────────────────────────────────────────────────

  /** Clicks the Group Study button in the header menu (only present when the feature is enabled). */
  async clickGroupStudy() {
    await this.page.locator('#groupStudyButton').click();
  }
}
