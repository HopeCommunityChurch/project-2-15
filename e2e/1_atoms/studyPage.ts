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

  /** Asserts the editor is NOT present (e.g. on not-found or not-authorized pages). */
  async assertNotVisible() {
    await expect(this.editor()).not.toBeVisible();
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

  /** Asserts the editor does NOT contain the given text anywhere in its content. */
  async assertNotContains(text: string) {
    await expect(this.editor()).not.toContainText(text);
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

  /** Asserts the given text is bold (wrapped in a strong element) in the editor. */
  async assertBoldActive(text: string) {
    await expect(this.editor().locator('strong', { hasText: text })).toBeVisible();
  }

  /** Asserts the given text is NOT bold (no strong element wraps it) in the editor. */
  async assertNotBoldActive(text: string) {
    await expect(this.editor().locator('strong', { hasText: text })).not.toBeVisible();
  }

  /** Asserts the given text is italic (wrapped in an em element) in the editor. */
  async assertItalicActive(text: string) {
    await expect(this.editor().locator('em', { hasText: text })).toBeVisible();
  }

  /** Asserts the given text is NOT italic (no em element wraps it) in the editor. */
  async assertNotItalicActive(text: string) {
    await expect(this.editor().locator('em', { hasText: text })).not.toBeVisible();
  }

  /** Asserts the given text is underlined (wrapped in a u element) in the editor. */
  async assertUnderlineActive(text: string) {
    await expect(this.editor().locator('u', { hasText: text })).toBeVisible();
  }

  /** Asserts the given text is NOT underlined (no u element wraps it) in the editor. */
  async assertNotUnderlineActive(text: string) {
    await expect(this.editor().locator('u', { hasText: text })).not.toBeVisible();
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

  // ── Structure content assertions ──────────────────────────────────────────

  /** Asserts the sidebar contains at least the given number of section items. */
  async assertSidebarSectionCountAtLeast(min: number) {
    const count = await this.page.locator('#leftSidebar .section').count();
    if (count < min) throw new Error(`Expected at least ${min} sidebar section items, found ${count}`);
  }

  /** Asserts the editor contains at least the given number of study block rows. */
  async assertStudyBlockCountAtLeast(min: number) {
    const count = await this.page.locator('.ProseMirror .studyBlocks tr[data-id]').count();
    if (count < min) throw new Error(`Expected at least ${min} study block rows, found ${count}`);
  }

  /** Asserts the editor contains at least the given number of question nodes. */
  async assertQuestionCountAtLeast(min: number) {
    const count = await this.page.locator('.ProseMirror questionouter').count();
    if (count < min) throw new Error(`Expected at least ${min} question nodes, found ${count}`);
  }

  /** Asserts the editor contains exactly the given number of bibleText (scripture) nodes. */
  async assertBibleTextCount(count: number) {
    const actual = await this.page.locator('.ProseMirror .bibleText').count();
    if (actual !== count) throw new Error(`Expected exactly ${count} bibleText nodes, found ${actual}`);
  }

  /** Asserts the editor contains exactly the given number of study block rows. */
  async assertStudyBlockCount(count: number) {
    const actual = await this.page.locator('.ProseMirror .studyBlocks tr[data-id]').count();
    if (actual !== count) throw new Error(`Expected exactly ${count} study block rows, found ${actual}`);
  }

  /** Asserts the editor contains exactly the given number of question nodes. */
  async assertQuestionCount(count: number) {
    const actual = await this.page.locator('.ProseMirror questionouter').count();
    if (actual !== count) throw new Error(`Expected exactly ${count} question nodes, found ${actual}`);
  }

  /** Asserts the sidebar contains exactly the given number of section items. */
  async assertSidebarSectionCount(count: number) {
    const actual = await this.page.locator('#leftSidebar .section').count();
    if (actual !== count) throw new Error(`Expected exactly ${count} sidebar section items, found ${actual}`);
  }

  /**
   * Clicks the text-body cell (2nd td > p) of the nth study block (0-based,
   * global across all sections) to place the cursor there for typing or
   * formatting. Selector: .studyBlocks tr[data-id] td:nth-child(2) p
   */
  async clickStudyBlockBody(blockIndex: number) {
    await this.page
      .locator('.ProseMirror .studyBlocks tr[data-id] td:nth-child(2) p')
      .nth(blockIndex)
      .click();
  }

  /**
   * Clicks the h2 heading of the nth section (0-based) to place the editor
   * cursor there. Use after addSection to establish cursor before subsequent
   * insertions. Selector: .ProseMirror .section:nth-child(N+1) h2
   */
  async clickSectionHeading(sectionIndex: number) {
    await this.page
      .locator('.ProseMirror .section h2')
      .nth(sectionIndex)
      .click();
  }

  /**
   * Clicks the "Remove" button (.deleter) on a sidebar section at the given
   * 0-based index to delete the section. The first click reveals the button,
   * the second confirms deletion.
   */
  async clickDeleteSection(sectionIndex: number) {
    // The .remove div (×) is display:none until the section is hovered.
    // Clicking it with force:true removes the section immediately.
    const section = this.page.locator('#leftSidebar .section').nth(sectionIndex);
    await section.hover();
    await section.locator('.remove').click({ force: true });
  }

  /**
   * Asserts the indent level (data-indent attribute) of a bibleText chunk at
   * a given index is at or above the expected level.
   * The scripture chunk's wrapper element carries `data-indent="<n>"`.
   */
  async assertScriptureChunkIndentLevel(index: number, level: number) {
    // The scripture chunk's inner `.chunk` element carries a `level` attribute
    // that starts at 0 and increases by 1 for each Indent action applied.
    const chunk = this.page.locator('.ProseMirror .bibleText').nth(index).locator('.chunk').first();
    await chunk.waitFor({ state: 'visible', timeout: 10_000 });
    const attr = await chunk.getAttribute('level');
    const actual = parseInt(attr ?? '0', 10);
    if (actual < level) {
      throw new Error(`Expected scripture chunk ${index} to have indent level >= ${level}, got ${actual}`);
    }
  }
}
