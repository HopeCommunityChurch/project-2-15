import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { typeAndSave } from '../2_molecules/typeAndSave';
import { addSection } from '../2_molecules/addSection';
import { addStudyBlock } from '../2_molecules/addStudyBlock';
import { addQuestion } from '../2_molecules/addQuestion';
import { deleteSection } from '../2_molecules/deleteSection';
import { insertScripture } from '../2_molecules/insertScripture';
import { applyBoldToText } from '../2_molecules/applyBoldToText';
import { applyItalicToText } from '../2_molecules/applyItalicToText';
import { applyUnderlineToText } from '../2_molecules/applyUnderlineToText';
import { undoChange } from '../2_molecules/undoChange';
import { redoChange } from '../2_molecules/redoChange';
import { goToStudies } from '../2_molecules/goToStudies';
import { openStudy } from '../2_molecules/openStudy';
import { openStudyFromStudiesPage } from '../2_molecules/openStudyFromStudiesPage';
import { randomUUID } from 'crypto';

test.describe('editor complex', () => {
  /**
   * Build a study with 3 additional sections, each containing typed content,
   * study blocks, questions, formatting, and scripture — then do a full page
   * reload and verify every piece of content and formatting survived the
   * save → close → reload cycle.
   *
   * ~43 operations.
   */
  test('multi-section study with formatting and scripture survives full page reload', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Reload ${randomUUID().slice(0, 8)}`;

    // ── Setup ──────────────────────────────────────────────────────────────────
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Section 1: plain text + study block + bold formatting ──────────────────
    const s1text = `s1-${randomUUID().slice(0, 8)}`;
    const s1block = `blk1-${randomUUID().slice(0, 8)}`;
    await addSection(page);                          // sidebar now has 2 sections (1 default + 1)
    await editor.typeAtEnd(s1text);
    await editor.assertContains(s1text);
    await addStudyBlock(page);
    await editor.assertStudyBlockCountAtLeast(1);
    await applyBoldToText(page, s1block);
    await editor.assertBoldActive(s1block);

    // ── Section 1: add a question with italic text ─────────────────────────────
    const s1q = `q1-${randomUUID().slice(0, 8)}`;
    await addQuestion(page);
    await editor.assertQuestionCountAtLeast(1);
    await applyItalicToText(page, s1q);
    await editor.assertItalicActive(s1q);

    // ── Section 2: scripture + study block + italic text ──────────────────────
    const s2text = `s2-${randomUUID().slice(0, 8)}`;
    const s2block = `blk2-${randomUUID().slice(0, 8)}`;
    await addSection(page);                          // sidebar now has 3 sections
    await editor.assertSidebarSectionCount(3);
    await editor.typeAtEnd(s2text);
    await editor.assertContains(s2text);
    await insertScripture(page, 'John 3:16');
    await editor.assertContains('For God so loved');
    await addStudyBlock(page);
    await editor.assertStudyBlockCountAtLeast(2);
    await applyItalicToText(page, s2block);
    await editor.assertItalicActive(s2block);

    // ── Section 2: question ────────────────────────────────────────────────────
    const s2q = `q2-${randomUUID().slice(0, 8)}`;
    await addQuestion(page);
    await editor.assertQuestionCountAtLeast(2);
    await applyBoldToText(page, s2q);
    await editor.assertBoldActive(s2q);

    // ── Section 3: underline formatting + scripture ────────────────────────────
    const s3text = `s3-${randomUUID().slice(0, 8)}`;
    await addSection(page);                          // sidebar now has 4 sections
    await editor.assertSidebarSectionCount(4);
    await editor.typeAtEnd(s3text);
    await editor.assertContains(s3text);
    await insertScripture(page, 'Genesis 1:1');
    await editor.assertContains('In the beginning');
    await applyUnderlineToText(page, s3text);
    await editor.assertUnderlineActive(s3text);

    // ── Wait for autosave then reload ─────────────────────────────────────────
    await editor.assertSaved();
    await page.reload();
    await page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 15_000 });

    // ── Verify all content survived the reload ────────────────────────────────
    await editor.assertSidebarSectionCount(4);
    await editor.assertContains(s1text);
    await editor.assertBoldActive(s1block);
    await editor.assertItalicActive(s1q);
    await editor.assertContains(s2text);
    await editor.assertContains('For God so loved');
    await editor.assertItalicActive(s2block);
    await editor.assertBoldActive(s2q);
    await editor.assertContains(s3text);
    await editor.assertContains('In the beginning');
    await editor.assertUnderlineActive(s3text);
    await editor.assertStudyBlockCountAtLeast(2);
    await editor.assertQuestionCountAtLeast(2);
  });

  /**
   * Build up content across sections, then undo each operation in reverse and
   * verify the editor state is consistent at each undo boundary. Finally redo
   * several operations and confirm they re-apply correctly.
   *
   * This exercises ProseMirror history batching with heterogeneous transactions
   * (marks + node inserts + structure), where the most common failure mode is
   * position corruption or mark attributes shifting to the wrong node.
   *
   * ~42 operations.
   */
  test('undo chain after multi-structure edits reverts to consistent state', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Undo ${randomUUID().slice(0, 8)}`;
    const s1text = `s1-${randomUUID().slice(0, 8)}`;
    const blockText = `blk-${randomUUID().slice(0, 8)}`;
    const qText = `q-${randomUUID().slice(0, 8)}`;
    const s2text = `s2-${randomUUID().slice(0, 8)}`;

    // ── Setup ──────────────────────────────────────────────────────────────────
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Build document ─────────────────────────────────────────────────────────
    await addSection(page);                          // section 1 added (total 2)
    await typeAndSave(page, s1text);
    await editor.assertContains(s1text);

    await addStudyBlock(page);
    await editor.assertStudyBlockCountAtLeast(1);
    await applyBoldToText(page, blockText);
    await editor.assertBoldActive(blockText);

    await addQuestion(page);
    await editor.assertQuestionCountAtLeast(1);
    await applyItalicToText(page, qText);
    await editor.assertItalicActive(qText);

    await insertScripture(page, 'Genesis 1:1');
    await editor.assertContains('In the beginning');

    await addSection(page);                          // section 2 added (total 3)
    await editor.assertSidebarSectionCount(3);
    await typeAndSave(page, s2text);
    await editor.assertContains(s2text);

    // ── Undo s2text typing ────────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertNotContains(s2text);

    // ── Undo section 2 add ────────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertSidebarSectionCount(2);

    // ── Undo scripture insert ─────────────────────────────────────────────────
    await undoChange(page);
    await editor.assertNotContains('In the beginning');

    // ── Undo italic on question text + question text itself + question add ─────
    await undoChange(page);   // undo italic application
    await undoChange(page);   // undo typing qText into question
    await undoChange(page);   // undo question add
    await editor.assertQuestionCount(0);

    // ── Undo bold on block text + block text itself + study block add ──────────
    await undoChange(page);   // undo bold application
    await undoChange(page);   // undo typing blockText into block
    await editor.assertNotContains(blockText);
    await undoChange(page);   // undo study block add
    await editor.assertStudyBlockCount(0);

    // ── Section 1 text should still be present (we only undid later ops) ───────
    await editor.assertContains(s1text);
    await editor.assertSidebarSectionCount(2);      // section 0 (default) + section 1

    // ── Redo: study block add ─────────────────────────────────────────────────
    await redoChange(page);
    await editor.assertStudyBlockCountAtLeast(1);

    // ── Redo: type blockText ──────────────────────────────────────────────────
    await redoChange(page);
    await editor.assertContains(blockText);

    // ── Redo: bold on blockText ───────────────────────────────────────────────
    await redoChange(page);
    await editor.assertBoldActive(blockText);

    // ── Redo: question add ────────────────────────────────────────────────────
    await redoChange(page);
    await editor.assertQuestionCountAtLeast(1);

    // ── Section 1 text unaffected by redo chain ───────────────────────────────
    await editor.assertContains(s1text);
    await editor.assertSidebarSectionCount(2);
  });

  /**
   * Build a section with deeply nested content (study block, question, scripture,
   * bold text) alongside a second plain section. Delete the rich section via the
   * sidebar. Assert all its nested content is gone and counts have zeroed out.
   * Then undo the deletion and assert every nested item is fully restored.
   *
   * This exercises ProseMirror's ability to serialize the full isolating node tree
   * (section → studyBlocks → questions; section → bibleText) into a single
   * reversible history step.
   *
   * ~41 operations.
   */
  test('delete section with nested content cascades fully and undo restores complete tree', async ({
    page,
    editor,
    freshUser,
  }) => {
    const title = `Complex Delete ${randomUUID().slice(0, 8)}`;
    const richText = `rich-${randomUUID().slice(0, 8)}`;
    const blockText = `blk-${randomUUID().slice(0, 8)}`;
    const qText = `q-${randomUUID().slice(0, 8)}`;
    const safeText = `safe-${randomUUID().slice(0, 8)}`;

    // ── Setup ──────────────────────────────────────────────────────────────────
    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Build the rich section (index 1) ──────────────────────────────────────
    await addSection(page);                          // total 2 sections (0-indexed: 0 and 1)
    await typeAndSave(page, richText);
    await editor.assertContains(richText);

    await addStudyBlock(page);
    await editor.assertStudyBlockCountAtLeast(1);
    await applyBoldToText(page, blockText);
    await editor.assertBoldActive(blockText);

    await addQuestion(page);
    await editor.assertQuestionCountAtLeast(1);
    await applyItalicToText(page, qText);
    await editor.assertItalicActive(qText);

    await insertScripture(page, 'Romans 8:28');
    await editor.assertContains('all things work together');

    // ── Add a plain section (index 2) that must survive the delete ────────────
    await addSection(page);                          // total 3 sections
    await editor.assertSidebarSectionCount(3);
    await typeAndSave(page, safeText);
    await editor.assertContains(safeText);

    // ── Verify pre-delete state ────────────────────────────────────────────────
    await editor.assertStudyBlockCountAtLeast(1);
    await editor.assertQuestionCountAtLeast(1);
    await editor.assertContains(richText);
    await editor.assertContains('all things work together');
    await editor.assertContains(safeText);

    // ── Delete the rich section (sidebar index 1) ─────────────────────────────
    await deleteSection(page, 1);
    await editor.assertSidebarSectionCount(2);

    // ── Assert full cascade: all nested content is gone ────────────────────────
    await editor.assertNotContains(richText);
    await editor.assertNotContains(blockText);
    await editor.assertNotContains(qText);
    await editor.assertNotContains('all things work together');
    await editor.assertStudyBlockCount(0);
    await editor.assertQuestionCount(0);

    // ── Plain section content must be unaffected ───────────────────────────────
    await editor.assertContains(safeText);

    // ── Undo the section deletion ──────────────────────────────────────────────
    await undoChange(page);
    await editor.assertSidebarSectionCount(3);

    // ── Assert full restore: every nested item is back ────────────────────────
    await editor.assertContains(richText);
    await editor.assertBoldActive(blockText);
    await editor.assertItalicActive(qText);
    await editor.assertContains('all things work together');
    await editor.assertStudyBlockCountAtLeast(1);
    await editor.assertQuestionCountAtLeast(1);

    // ── Plain section must still be intact ────────────────────────────────────
    await editor.assertContains(safeText);
  });

  /**
   * Create two studies with distinct content, navigate between them multiple
   * times, and assert that neither study ever shows content from the other.
   *
   * The frontend attaches the ProseMirror view to a global `window.editor` and
   * uses a `wasInitialized` flag plus a WebSocket `DocOpened` handshake to load
   * each study's content. If either is not correctly torn down and re-initialized
   * on navigation, editor state leaks between studies — the second study shows
   * the first study's text, structure, or formatting.
   *
   * ~51 operations.
   */
  test('navigating between multiple studies isolates editor state per study', async ({
    page,
    editor,
    freshUser,
  }) => {
    const titleA = `Complex Nav-A ${randomUUID().slice(0, 8)}`;
    const titleB = `Complex Nav-B ${randomUUID().slice(0, 8)}`;
    const textA1 = `nav-a1-${randomUUID().slice(0, 8)}`;
    const textA2 = `nav-a2-${randomUUID().slice(0, 8)}`;
    const boldA = `bold-a-${randomUUID().slice(0, 8)}`;
    const textB1 = `nav-b1-${randomUUID().slice(0, 8)}`;
    const textB2 = `nav-b2-${randomUUID().slice(0, 8)}`;

    // ── Build Study A ─────────────────────────────────────────────────────────
    await login(page, freshUser.email, freshUser.password);
    const { studyId: studyIdA } = await createStudy(page, titleA);
    await editor.assertVisible();
    await editor.assertTitle(titleA);

    await addSection(page);
    await editor.assertSidebarSectionCount(2);
    await typeAndSave(page, textA1);
    await editor.assertContains(textA1);

    await applyBoldToText(page, boldA);
    await editor.assertBoldActive(boldA);

    await addSection(page);
    await editor.assertSidebarSectionCount(3);
    await typeAndSave(page, textA2);
    await editor.assertContains(textA2);
    await editor.assertSaved();

    // ── Navigate away to studies, create Study B ───────────────────────────────
    await goToStudies(page);
    const { studyId: studyIdB } = await createStudy(page, titleB);
    await editor.assertVisible();

    // B must load its own blank state — not A's
    await editor.assertTitle(titleB);
    await editor.assertNotContains(textA1);
    await editor.assertNotContains(textA2);
    await editor.assertNotContains(boldA);
    await editor.assertSidebarSectionCount(1);   // B has only its 1 default section

    await typeAndSave(page, textB1);
    await editor.assertContains(textB1);
    await addSection(page);
    await typeAndSave(page, textB2);
    await editor.assertContains(textB2);
    await editor.assertSidebarSectionCount(2);
    await editor.assertSaved();

    // ── Return to Study A and verify it was not corrupted by B ────────────────
    await goToStudies(page);
    await openStudyFromStudiesPage(page, titleA);
    await editor.assertTitle(titleA);
    await editor.assertContains(textA1);
    await editor.assertContains(textA2);
    await editor.assertBoldActive(boldA);
    await editor.assertSidebarSectionCount(3);
    await editor.assertNotContains(textB1);
    await editor.assertNotContains(textB2);

    // ── Return to Study B and verify it was not corrupted by reopening A ───────
    await goToStudies(page);
    await openStudyFromStudiesPage(page, titleB);
    await editor.assertTitle(titleB);
    await editor.assertContains(textB1);
    await editor.assertContains(textB2);
    await editor.assertSidebarSectionCount(2);
    await editor.assertNotContains(textA1);
    await editor.assertNotContains(textA2);

    // ── Direct URL navigation back to A (bypasses studies list) ──────────────
    await openStudy(page, studyIdA);
    await editor.assertTitle(titleA);
    await editor.assertContains(textA1);
    await editor.assertContains(textA2);
    await editor.assertBoldActive(boldA);
    await editor.assertNotContains(textB1);
    await editor.assertNotContains(textB2);

    // ── Immediately navigate by direct URL to B (rapid context switch) ────────
    await openStudy(page, studyIdB);
    await editor.assertTitle(titleB);
    await editor.assertContains(textB1);
    await editor.assertContains(textB2);
    await editor.assertNotContains(textA1);
    await editor.assertNotContains(boldA);
  });

  /**
   * Insert five scripture passages across two sections with a study block
   * present, assert they remain as separate nodes (never merge), then indent
   * the last passage and verify indentation survives a full page reload.
   *
   * Tests scripture-specific failure modes: node merging on rapid sequential
   * inserts, incorrect placement when a study block is present, and whether
   * the `level` attr on chunk nodes survives the JSONB save/reload cycle.
   *
   * ~39 operations.
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

    // ── Insert three scripture passages in sequence ────────────────────────────
    await insertScripture(page, 'John 3:16');
    await editor.assertContains('For God so loved');
    await editor.assertBibleTextCount(1);

    await insertScripture(page, 'Genesis 1:1');
    await editor.assertContains('In the beginning');
    await editor.assertBibleTextCount(2);

    await insertScripture(page, 'Romans 8:28');
    await editor.assertContains('all things work together');
    await editor.assertBibleTextCount(3);

    // ── Add a study block then insert another scripture ───────────────────────
    // scripture placement logic searches for studyBlocks and inserts before it;
    // this verifies the fourth passage lands in the document, not lost.
    await addStudyBlock(page);
    await editor.assertStudyBlockCountAtLeast(1);
    await insertScripture(page, '1 Corinthians 13:4');
    await editor.assertContains('Love is patient');
    await editor.assertBibleTextCount(4);
    await editor.assertStudyBlockCountAtLeast(1);   // block survived the insert

    // ── Add a new section and insert a fifth scripture in it ──────────────────
    await addSection(page);
    await editor.assertSidebarSectionCount(2);
    await insertScripture(page, 'Psalm 23:1');
    await editor.assertContains('The LORD is my shepherd');
    await editor.assertBibleTextCount(5);
    await editor.assertSidebarSectionCount(2);      // section count unchanged

    // ── Indent the fifth scripture chunk twice, then outdent once ─────────────
    // Inline navigation to click into the scripture node — same pattern as
    // document-persistence.spec.ts which clicks .bibleText .chunk directly.
    await page.locator('.ProseMirror .bibleText').nth(4).locator('.chunk').first().click();
    await editor.clickIndent();
    await editor.assertScriptureChunkIndentLevel(4, 1);
    await editor.clickIndent();
    await editor.assertScriptureChunkIndentLevel(4, 2);
    await editor.clickOutdent();
    await editor.assertScriptureChunkIndentLevel(4, 1);

    // ── Reload and verify everything persisted ────────────────────────────────
    await editor.assertSaved();
    await page.reload();
    await page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 15_000 });

    await editor.assertBibleTextCount(5);
    await editor.assertContains('For God so loved');
    await editor.assertContains('In the beginning');
    await editor.assertContains('all things work together');
    await editor.assertContains('Love is patient');
    await editor.assertContains('The LORD is my shepherd');
    await editor.assertScriptureChunkIndentLevel(4, 1);   // indentation survived JSONB round-trip
    await editor.assertSidebarSectionCount(2);
    await editor.assertStudyBlockCountAtLeast(1);
  });

  /**
   * Apply bold, italic, and underline simultaneously to the same text, verify
   * all three marks coexist, then clear all formatting and verify all three
   * are removed in one operation. Undo the clear and verify all three restore.
   * Then verify marks applied to separate blocks don't bleed into each other,
   * and that undoing formatting from one block leaves other blocks unaffected.
   *
   * Tests ProseMirror's multi-mark coexistence, clearFormatting atomicity,
   * undo/redo of a clear-all-marks transaction, and mark boundary isolation.
   *
   * ~42 operations.
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
    const text4 = `underline-only-${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    await createStudy(page, title);
    await editor.assertVisible();

    // ── Phase 1: Apply all three marks to the same text ───────────────────────
    // Use a single study block text cell; re-select between each mark application
    // to ensure each toggleMark transaction is applied to the full text range.
    await addStudyBlock(page);
    const textCell = page.locator('.ProseMirror .studyBlocks tr[data-id] td:nth-child(2) p').first();
    await textCell.click();
    await page.keyboard.type(tripleText);

    // Bold
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickBold();
    await page.locator('.ProseMirror strong', { hasText: tripleText }).waitFor({ state: 'visible', timeout: 10_000 });

    // Italic (re-select so toggleMark applies to full range)
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickItalic();
    await page.locator('.ProseMirror em', { hasText: tripleText }).waitFor({ state: 'visible', timeout: 10_000 });

    // Underline (re-select)
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    await page.keyboard.press('End');
    await page.keyboard.up('Shift');
    await editor.clickUnderline();
    await page.locator('.ProseMirror u', { hasText: tripleText }).waitFor({ state: 'visible', timeout: 10_000 });

    // All three marks must coexist on the same text
    await editor.assertBoldActive(tripleText);
    await editor.assertItalicActive(tripleText);
    await editor.assertUnderlineActive(tripleText);

    // ── Phase 2: Type new text after the triple-marked text ───────────────────
    // New text typed after the cursor must not inherit any of the three marks.
    await page.keyboard.press('End');
    await page.keyboard.type(suffix);
    await editor.assertContains(suffix);
    await editor.assertNotBoldActive(suffix);
    await editor.assertNotItalicActive(suffix);
    await editor.assertNotUnderlineActive(suffix);
    await editor.assertBoldActive(tripleText);    // original marks still intact

    // ── Phase 3: Clear all marks from tripleText ──────────────────────────────
    // Re-select tripleText only (character-by-character to stay within the
    // marked span and not include the unformatted suffix).
    await page.keyboard.press('Home');
    await page.keyboard.down('Shift');
    for (let i = 0; i < tripleText.length; i++) await page.keyboard.press('ArrowRight');
    await page.keyboard.up('Shift');
    await editor.clickClearFormatting();
    await page.locator('.ProseMirror strong', { hasText: tripleText }).waitFor({ state: 'hidden', timeout: 10_000 });

    await editor.assertNotBoldActive(tripleText);
    await editor.assertNotItalicActive(tripleText);
    await editor.assertNotUnderlineActive(tripleText);
    await editor.assertContains(tripleText);    // content still present after clear

    // ── Phase 4: Undo clear formatting → all three marks restore ─────────────
    await undoChange(page);
    await editor.assertBoldActive(tripleText);
    await editor.assertItalicActive(tripleText);
    await editor.assertUnderlineActive(tripleText);

    // ── Phase 5: Redo clear formatting → all three marks gone again ───────────
    await redoChange(page);
    await editor.assertNotBoldActive(tripleText);
    await editor.assertNotItalicActive(tripleText);
    await editor.assertNotUnderlineActive(tripleText);

    // ── Phase 6: Mark boundary isolation across separate blocks ───────────────
    // Each block gets exactly one mark type; none must bleed into the others.
    await applyBoldToText(page, text2);
    await editor.assertBoldActive(text2);
    await editor.assertNotItalicActive(text2);
    await editor.assertNotUnderlineActive(text2);

    await applyItalicToText(page, text3);
    await editor.assertItalicActive(text3);
    await editor.assertNotBoldActive(text3);
    await editor.assertNotUnderlineActive(text3);

    await applyUnderlineToText(page, text4);
    await editor.assertUnderlineActive(text4);
    await editor.assertNotBoldActive(text4);
    await editor.assertNotItalicActive(text4);

    // ── Phase 7: Undo text4 formatting; text2 and text3 must remain intact ────
    await undoChange(page);    // undo underline on text4
    await editor.assertNotUnderlineActive(text4);
    await editor.assertBoldActive(text2);         // text2's bold must survive
    await editor.assertItalicActive(text3);       // text3's italic must survive

    await undoChange(page);    // undo text4 typing
    await editor.assertNotContains(text4);
    await editor.assertBoldActive(text2);         // still intact
    await editor.assertItalicActive(text3);       // still intact
  });
});
