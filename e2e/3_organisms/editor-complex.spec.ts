import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { addSection } from '../2_molecules/addSection';
import { addStudyBlock } from '../2_molecules/addStudyBlock';
import { addQuestion } from '../2_molecules/addQuestion';
import { deleteSection } from '../2_molecules/deleteSection';
import { insertScripture } from '../2_molecules/insertScripture';
import { undoChange } from '../2_molecules/undoChange';
import { redoChange } from '../2_molecules/redoChange';
import { goToStudies } from '../2_molecules/goToStudies';
import { openStudy } from '../2_molecules/openStudy';
import { openStudyFromStudiesPage } from '../2_molecules/openStudyFromStudiesPage';
import { randomUUID } from 'crypto';

// ── Cursor model ────────────────────────────────────────────────────────────
//
//   After createStudy:   cursor at section 0 h2 (sectionHeader)
//   After addSection:    cursor at new section's h2 (set by addSection transaction)
//                        BUT sidebar click may defocus editor — always call
//                        editor.clickSectionHeading(N) to re-establish cursor.
//   After insertScripture: cursor unchanged (same section h2)
//   After addStudyBlock: cursor unchanged (still in section h2)
//   After clickStudyBlockBody(N): cursor in Nth block's body <td> paragraph
//   After keyboard.type(): cursor at end of typed text
//
//   RULE: insertScripture before addStudyBlock in every section.
//         addStudyBlock uses $anchor.node(1) to find the section — cursor
//         must be somewhere inside the target section.
//
// ────────────────────────────────────────────────────────────────────────────

test.describe('editor complex', () => {
  /**
   * Build a 3-section study: scripture + study block + formatted body text in
   * each section. Reload and verify every piece of content and formatting
   * survived the save → close → reload cycle.
   *
   * Tests that bibleText, studyBlock rows, mark serialisation, and section
   * structure all survive the JSONB round-trip.
   *
   * ~45 operations.
   */
  test('multi-section study with formatting and scripture survives full page reload', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Reload ${randomUUID().slice(0, 8)}`;
    const boldText = `bold-${randomUUID().slice(0, 8)}`;
    const italicText = `italic-${randomUUID().slice(0, 8)}`;
    const underlineText = `under-${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Section 0 (default): scripture → study block → bold body text ─────────
    // Cursor starts at section 0 h2 on document load.
    await editor.clickSectionHeading(0);
    await insertScripture(page, 'John 3:16');
    await editor.assertBibleTextCount(1);
    const { blockIndex: b0 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b0);
    await page.keyboard.type(boldText);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickBold();
    await page.locator('.ProseMirror strong', { hasText: boldText }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertBoldActive(boldText);

    // ── Section 1: scripture → study block → italic body text ─────────────────
    await addSection(page);                            // cursor: section 1 h2
    await editor.clickSectionHeading(1);               // re-establish cursor after sidebar click
    await insertScripture(page, 'Genesis 1:1');
    await editor.assertBibleTextCount(2);
    await editor.assertSidebarSectionCount(2);
    const { blockIndex: b1 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b1);
    await page.keyboard.type(italicText);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickItalic();
    await page.locator('.ProseMirror em', { hasText: italicText }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertItalicActive(italicText);

    // ── Section 2: scripture → study block → underline body text ──────────────
    await addSection(page);                            // cursor: section 2 h2
    await editor.clickSectionHeading(2);
    await insertScripture(page, 'Romans 8:28');
    await editor.assertBibleTextCount(3);
    await editor.assertSidebarSectionCount(3);
    const { blockIndex: b2 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b2);
    await page.keyboard.type(underlineText);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickUnderline();
    await page.locator('.ProseMirror u', { hasText: underlineText }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertUnderlineActive(underlineText);

    // ── Autosave → reload ─────────────────────────────────────────────────────
    await editor.assertSaved();
    await page.reload();
    await page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 15_000 });

    // ── Verify everything survived ─────────────────────────────────────────────
    await editor.assertSidebarSectionCount(3);
    await editor.assertBibleTextCount(3);
    await editor.assertContains('For God so loved');
    await editor.assertContains('In the beginning');
    await editor.assertContains('all things work together');
    await editor.assertBoldActive(boldText);
    await editor.assertItalicActive(italicText);
    await editor.assertUnderlineActive(underlineText);
    await editor.assertStudyBlockCountAtLeast(3);
  });

  /**
   * Build scripture + study block + bold text in section 0, then a second
   * section with scripture + study block. Undo each operation in reverse and
   * verify the editor state is consistent at each step. Then redo several
   * operations and confirm they re-apply correctly.
   *
   * Tests ProseMirror history batching across heterogeneous transactions.
   *
   * ~40 operations.
   */
  test('undo chain after multi-structure edits reverts to consistent state', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Undo ${randomUUID().slice(0, 8)}`;
    const boldText = `bold-${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Section 0: scripture → study block → bold text ─────────────────────────
    await editor.clickSectionHeading(0);
    await insertScripture(page, 'John 3:16');
    await editor.assertBibleTextCount(1);
    const { blockIndex: b0 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b0);
    await page.keyboard.type(boldText);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickBold();
    await page.locator('.ProseMirror strong', { hasText: boldText }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertBoldActive(boldText);

    // ── Section 1: scripture only ───────────────────────────────────────────────
    await addSection(page);
    await editor.clickSectionHeading(1);
    await insertScripture(page, 'Genesis 1:1');
    await editor.assertBibleTextCount(2);
    await editor.assertSidebarSectionCount(2);
    const { blockIndex: b1 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b1);
    // Cursor is in section 1's block — mark this state for undo tests

    // ── Undo: section 1 study block add ────────────────────────────────────────
    await undoChange(page);
    await editor.assertStudyBlockCount(1);

    // ── Undo: section 1 scripture ──────────────────────────────────────────────
    await undoChange(page);
    await editor.assertBibleTextCount(1);
    await editor.assertNotContains('In the beginning');

    // ── Undo: section 1 add ────────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertSidebarSectionCount(1);

    // ── Section 0 content must still be intact ─────────────────────────────────
    await editor.assertContains('For God so loved');
    await editor.assertBoldActive(boldText);
    await editor.assertBibleTextCount(1);
    await editor.assertStudyBlockCount(1);

    // ── Undo: bold on boldText ─────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertNotBoldActive(boldText);

    // ── Undo: boldText typing ──────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertNotContains(boldText);

    // ── Undo: study block add ──────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertStudyBlockCount(0);

    // ── Undo: scripture insert ─────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertBibleTextCount(0);
    await editor.assertNotContains('For God so loved');

    // ── Redo: scripture ────────────────────────────────────────────────────────
    await redoChange(page);
    await editor.assertBibleTextCount(1);
    await editor.assertContains('For God so loved');

    // ── Redo: study block add ──────────────────────────────────────────────────
    await redoChange(page);
    await editor.assertStudyBlockCountAtLeast(1);

    // ── Redo: boldText typing ──────────────────────────────────────────────────
    await redoChange(page);
    await editor.assertContains(boldText);

    // ── Redo: bold ─────────────────────────────────────────────────────────────
    await redoChange(page);
    await editor.assertBoldActive(boldText);

    // Section 1 was not redone — still 1 section
    await editor.assertSidebarSectionCount(1);
  });

  /**
   * Build a section with scripture, a study block, a question, and bold
   * formatting alongside a plain section. Delete the rich section via the
   * sidebar. Assert all its nested content is gone and counts have zeroed out.
   * Then undo and verify everything is fully restored.
   *
   * Tests ProseMirror's ability to serialise the full isolating node tree
   * into a single reversible history step.
   *
   * ~40 operations.
   */
  test('delete section with nested content cascades fully and undo restores complete tree', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Delete ${randomUUID().slice(0, 8)}`;
    const richText = `rich-${randomUUID().slice(0, 8)}`;
    const safeText = `safe-${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Section 0: plain safe content ─────────────────────────────────────────
    await editor.clickSectionHeading(0);
    await insertScripture(page, 'Psalm 23:1');
    await editor.assertBibleTextCount(1);
    const { blockIndex: b0 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b0);
    await page.keyboard.type(safeText);
    await editor.assertContains(safeText);

    // ── Section 1 (rich): scripture → study block → bold → question ────────────
    await addSection(page);
    await editor.clickSectionHeading(1);
    await insertScripture(page, 'Romans 8:28');
    await editor.assertBibleTextCount(2);
    await editor.assertSidebarSectionCount(2);
    const { blockIndex: b1 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b1);
    await page.keyboard.type(richText);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickBold();
    await page.locator('.ProseMirror strong', { hasText: richText }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertBoldActive(richText);

    // Add a question to section 1 (cursor is in section 1's block body)
    await addQuestion(page);
    await editor.assertQuestionCountAtLeast(1);

    // ── Pre-delete verification ────────────────────────────────────────────────
    await editor.assertStudyBlockCount(2);
    await editor.assertContains(safeText);
    await editor.assertContains(richText);
    await editor.assertContains('all things work together');
    await editor.assertContains('The LORD is my shepherd');

    // ── Delete section 1 (the rich section) via sidebar ────────────────────────
    await deleteSection(page, 1);
    await editor.assertSidebarSectionCount(1);

    // ── Full cascade: section 1 content must be gone ──────────────────────────
    await editor.assertNotContains(richText);
    await editor.assertNotContains('all things work together');
    await editor.assertStudyBlockCount(1);    // only section 0's block remains
    await editor.assertQuestionCount(0);

    // ── Section 0 content must be unaffected ──────────────────────────────────
    await editor.assertContains(safeText);
    await editor.assertContains('The LORD is my shepherd');
    await editor.assertBibleTextCount(1);

    // ── Undo the section deletion ──────────────────────────────────────────────
    await undoChange(page);
    await editor.assertSidebarSectionCount(2);

    // ── Full restore: section 1 content is back ───────────────────────────────
    await editor.assertContains(richText);
    await editor.assertBoldActive(richText);
    await editor.assertContains('all things work together');
    await editor.assertBibleTextCount(2);
    await editor.assertStudyBlockCount(2);
    await editor.assertQuestionCountAtLeast(1);

    // ── Section 0 still intact after undo ─────────────────────────────────────
    await editor.assertContains(safeText);
  });

  /**
   * Create two studies with distinct content, navigate between them multiple
   * times via the studies list and direct URL, and assert that neither study
   * ever shows content from the other.
   *
   * Tests that window.editor / wasInitialized / WebSocket DocOpened state is
   * correctly torn down and re-initialised on each navigation.
   *
   * ~50 operations.
   */
  test('navigating between multiple studies isolates editor state per study', async ({
    page,
    editor,
    freshUser,
  }) => {
    const titleA = `Complex Nav-A ${randomUUID().slice(0, 8)}`;
    const titleB = `Complex Nav-B ${randomUUID().slice(0, 8)}`;
    const textA = `nav-a-${randomUUID().slice(0, 8)}`;
    const textB = `nav-b-${randomUUID().slice(0, 8)}`;

    // ── Build Study A: scripture + study block + bold text ────────────────────
    await login(page, freshUser.email, freshUser.password);
    const { studyId: studyIdA } = await createStudy(page, titleA);
    await editor.assertVisible();
    await editor.assertTitle(titleA);

    await editor.clickSectionHeading(0);
    await insertScripture(page, 'John 3:16');
    await editor.assertBibleTextCount(1);
    const { blockIndex: bA0 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(bA0);
    await page.keyboard.type(textA);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickBold();
    await page.locator('.ProseMirror strong', { hasText: textA }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertBoldActive(textA);

    // Add a second section to A so it has distinct structure
    await addSection(page);
    await editor.clickSectionHeading(1);
    await insertScripture(page, 'Genesis 1:1');
    await editor.assertBibleTextCount(2);
    await editor.assertSidebarSectionCount(2);
    await editor.assertSaved();

    // ── Navigate away; create Study B ─────────────────────────────────────────
    await goToStudies(page);
    const { studyId: studyIdB } = await createStudy(page, titleB);
    await editor.assertVisible();
    await editor.assertTitle(titleB);

    // B must start blank — none of A's content should appear
    await editor.assertNotContains(textA);
    await editor.assertNotContains('For God so loved');
    await editor.assertSidebarSectionCount(1);

    await editor.clickSectionHeading(0);
    await insertScripture(page, 'Psalm 23:1');
    await editor.assertBibleTextCount(1);
    const { blockIndex: bB0 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(bB0);
    await page.keyboard.type(textB);
    await editor.assertContains(textB);
    await editor.assertSaved();

    // ── Return to Study A — verify it was not corrupted by B ──────────────────
    await goToStudies(page);
    await openStudyFromStudiesPage(page, titleA);
    await editor.assertTitle(titleA);
    await editor.assertContains(textA);
    await editor.assertBoldActive(textA);
    await editor.assertContains('For God so loved');
    await editor.assertContains('In the beginning');
    await editor.assertBibleTextCount(2);
    await editor.assertSidebarSectionCount(2);
    await editor.assertNotContains(textB);

    // ── Return to Study B — verify it was not corrupted by reopening A ────────
    await goToStudies(page);
    await openStudyFromStudiesPage(page, titleB);
    await editor.assertTitle(titleB);
    await editor.assertContains(textB);
    await editor.assertContains('The LORD is my shepherd');
    await editor.assertBibleTextCount(1);
    await editor.assertSidebarSectionCount(1);
    await editor.assertNotContains(textA);

    // ── Direct URL navigation to A (bypasses the studies list) ───────────────
    await openStudy(page, studyIdA);
    await editor.assertTitle(titleA);
    await editor.assertContains(textA);
    await editor.assertBoldActive(textA);
    await editor.assertNotContains(textB);

    // ── Immediate direct URL switch to B (rapid context switch) ───────────────
    await openStudy(page, studyIdB);
    await editor.assertTitle(titleB);
    await editor.assertContains(textB);
    await editor.assertNotContains(textA);
  });

  /**
   * Insert five scripture passages across two sections with a study block
   * present. Verify they stay as separate bibleText nodes, indent the last
   * passage twice then outdent once, and verify indentation survives a full
   * page reload.
   *
   * Tests scripture-specific failure modes: node merging, incorrect placement
   * when a study block is present, and whether the `level` attr survives the
   * JSONB save/reload cycle.
   *
   * ~40 operations.
   */
  test('multiple scripture passages stay separate and indentation survives reload', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Scripture ${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Section 0: three scripture passages then a study block ────────────────
    await editor.clickSectionHeading(0);
    await insertScripture(page, 'John 3:16');
    await editor.assertContains('For God so loved');
    await editor.assertBibleTextCount(1);

    await insertScripture(page, 'Genesis 1:1');
    await editor.assertContains('In the beginning');
    await editor.assertBibleTextCount(2);

    await insertScripture(page, 'Romans 8:28');
    await editor.assertContains('all things work together');
    await editor.assertBibleTextCount(3);

    // Add a study block after the three passages — insertion of further
    // scripture must still land in the correct section node.
    await addStudyBlock(page);
    await editor.assertStudyBlockCountAtLeast(1);

    // Click back to the section heading so insertScripture uses section 0
    await editor.clickSectionHeading(0);
    await insertScripture(page, '1 Corinthians 13:4');
    await editor.assertContains('Love is patient');
    await editor.assertBibleTextCount(4);
    await editor.assertStudyBlockCountAtLeast(1);   // block survived the insert

    // ── Section 1: fifth scripture ────────────────────────────────────────────
    await addSection(page);
    await editor.clickSectionHeading(1);
    await editor.assertSidebarSectionCount(2);
    await insertScripture(page, 'Psalm 23:1');
    await editor.assertContains('The LORD is my shepherd');
    await editor.assertBibleTextCount(5);

    // ── Indent the fifth passage twice, outdent once ──────────────────────────
    await page.locator('.ProseMirror .bibleText').nth(4).locator('.chunk').first().click();
    await editor.clickIndent();
    await editor.assertScriptureChunkIndentLevel(4, 1);
    await editor.clickIndent();
    await editor.assertScriptureChunkIndentLevel(4, 2);
    await editor.clickOutdent();
    await editor.assertScriptureChunkIndentLevel(4, 1);

    // ── Reload and verify persistence ────────────────────────────────────────
    await editor.assertSaved();
    await page.reload();
    await page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 15_000 });

    await editor.assertBibleTextCount(5);
    await editor.assertContains('For God so loved');
    await editor.assertContains('In the beginning');
    await editor.assertContains('all things work together');
    await editor.assertContains('Love is patient');
    await editor.assertContains('The LORD is my shepherd');
    await editor.assertScriptureChunkIndentLevel(4, 1);   // level survived JSONB round-trip
    await editor.assertSidebarSectionCount(2);
    await editor.assertStudyBlockCountAtLeast(1);
  });

  /**
   * Apply bold, italic, and underline simultaneously to the same text, verify
   * all three coexist, then clear all formatting and verify all three are
   * removed. Undo the clear and verify all three restore. Redo and verify
   * all three are gone again. Then verify marks applied to separate blocks
   * don't bleed into each other.
   *
   * Tests ProseMirror multi-mark coexistence, clearFormatting atomicity,
   * undo/redo of a clear-all-marks transaction, and mark boundary isolation.
   *
   * ~45 operations.
   */
  test('triple-mark text cleared and restored by undo, marks stay isolated across blocks', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Format ${randomUUID().slice(0, 8)}`;
    const tripleText = `triple-${randomUUID().slice(0, 8)}`;
    const suffix = `suffix-${randomUUID().slice(0, 8)}`;
    const text2 = `bold-only-${randomUUID().slice(0, 8)}`;
    const text3 = `italic-only-${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Scripture first, then study block ─────────────────────────────────────
    await editor.clickSectionHeading(0);
    await insertScripture(page, 'John 3:16');
    await editor.assertBibleTextCount(1);

    // Block 0: the triple-mark target
    const { blockIndex: b0 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b0);
    await page.keyboard.type(tripleText);

    // ── Phase 1: apply all three marks to tripleText ──────────────────────────
    // Re-select before each mark so the toggle applies to the full range.
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickBold();
    await page.locator('.ProseMirror strong', { hasText: tripleText }).waitFor({ state: 'visible', timeout: 10_000 });

    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickItalic();
    await page.locator('.ProseMirror em', { hasText: tripleText }).waitFor({ state: 'visible', timeout: 10_000 });

    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickUnderline();
    await page.locator('.ProseMirror u', { hasText: tripleText }).waitFor({ state: 'visible', timeout: 10_000 });

    await editor.assertBoldActive(tripleText);
    await editor.assertItalicActive(tripleText);
    await editor.assertUnderlineActive(tripleText);

    // ── Phase 2: suffix typed after tripleText must not inherit marks ─────────
    await page.keyboard.press('End');
    await page.keyboard.type(suffix);
    await editor.assertContains(suffix);
    await editor.assertNotBoldActive(suffix);
    await editor.assertNotItalicActive(suffix);
    await editor.assertNotUnderlineActive(suffix);
    await editor.assertBoldActive(tripleText);    // original marks still intact

    // ── Phase 3: clear all marks from tripleText only ─────────────────────────
    // Move to start and select exactly tripleText characters.
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    for (let i = 0; i < tripleText.length; i++) await page.keyboard.press('ArrowRight');
    await page.keyboard.up('Shift');
    await editor.clickClearFormatting();
    await page.locator('.ProseMirror strong', { hasText: tripleText }).waitFor({ state: 'hidden', timeout: 10_000 });

    await editor.assertNotBoldActive(tripleText);
    await editor.assertNotItalicActive(tripleText);
    await editor.assertNotUnderlineActive(tripleText);
    await editor.assertContains(tripleText);    // text content survives clear

    // ── Phase 4: undo clear → all three marks restore ─────────────────────────
    await undoChange(page);
    await editor.assertBoldActive(tripleText);
    await editor.assertItalicActive(tripleText);
    await editor.assertUnderlineActive(tripleText);

    // ── Phase 5: redo clear → all three marks gone again ──────────────────────
    await redoChange(page);
    await editor.assertNotBoldActive(tripleText);
    await editor.assertNotItalicActive(tripleText);
    await editor.assertNotUnderlineActive(tripleText);

    // ── Phase 6: mark boundary isolation — separate blocks, one mark each ─────
    // Block 1: bold only
    const { blockIndex: b1 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b1);
    await page.keyboard.type(text2);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickBold();
    await page.locator('.ProseMirror strong', { hasText: text2 }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertBoldActive(text2);
    await editor.assertNotItalicActive(text2);
    await editor.assertNotUnderlineActive(text2);

    // Block 2: italic only
    const { blockIndex: b2 } = await addStudyBlock(page);
    await editor.clickStudyBlockBody(b2);
    await page.keyboard.type(text3);
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickItalic();
    await page.locator('.ProseMirror em', { hasText: text3 }).waitFor({ state: 'visible', timeout: 10_000 });
    await editor.assertItalicActive(text3);
    await editor.assertNotBoldActive(text3);
    await editor.assertNotUnderlineActive(text3);

    // Block 0 and block 1 marks must still be intact after block 2 was formatted
    await editor.assertBoldActive(text2);        // block 1 bold survived
    await editor.assertNotBoldActive(tripleText); // block 0 still cleared

    // ── Phase 7: undo italic on block 2; block 1 must remain intact ───────────
    await undoChange(page);    // undo italic on text3
    await editor.assertNotItalicActive(text3);
    await editor.assertBoldActive(text2);         // block 1's bold must survive

    await undoChange(page);    // undo text3 typing
    await editor.assertNotContains(text3);
    await editor.assertBoldActive(text2);         // still intact
  });
});
