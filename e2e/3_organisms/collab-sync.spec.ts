/**
 * collab-sync — multi-tab real-time collaboration tests.
 *
 * Each test opens the same study document in two browser contexts (same user,
 * isolated cookie jars) and asserts that edits made in one tab propagate to
 * the other via the WebSocket OT broadcast path (DocUpdated).
 *
 * Test 5 (save-indicator propagation to remote tab) is intentionally omitted:
 * the backend sends OutDocSaved only to the tab that issued SaveDoc, not to
 * all open editors. There is nothing to assert on the remote tab.
 */

import { test, EditorAtoms } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { createStudy } from '../2_molecules/createStudy';
import { openStudy } from '../2_molecules/openStudy';
import { randomUUID } from 'crypto';
import type { BrowserContext } from '@playwright/test';

// WS sync SLA: remote steps should appear within 3 seconds of being confirmed
// by the backend on the simple (no-conflict) broadcast path.
const WS_TIMEOUT_MS = 3_000;

// The OT conflict cycle (DocConflict → client rebase → retransmit →
// DocConfirmed → DocUpdated) requires extra round-trips. Allow 8 seconds
// for convergence in the concurrent-edit test.
const OT_CONFLICT_TIMEOUT_MS = 8_000;

test.describe('collab sync', () => {
  let ctx2: BrowserContext | undefined;

  test.afterEach(async () => {
    if (ctx2) {
      await ctx2.close();
      ctx2 = undefined;
    }
  });

  test('tab 1 types, tab 2 receives the update via DocUpdated', async ({
    page,
    editor,
    browser,
    freshUser,
  }) => {
    const title = `Collab T1toT2 ${randomUUID().slice(0, 8)}`;
    const typed = `t1-${randomUUID().slice(0, 8)}`;

    // Tab 1: log in, create study, editor is ready.
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await editor.assertVisible();

    // Tab 2: same user, isolated cookie jar, navigate to the same study.
    ctx2 = await browser.newContext();
    const page2 = await ctx2.newPage();
    const editor2 = new EditorAtoms(page2);
    await login(page2, freshUser.email, freshUser.password);
    await openStudy(page2, studyId);
    await editor2.assertVisible();

    // Tab 1 types — triggers Updated → DocConfirmed (Tab 1) + DocUpdated (Tab 2).
    await editor.typeAtEnd(typed);

    // Tab 2 must receive and apply the remote steps within the WS SLA.
    await editor2.assertContainsWithTimeout(typed, WS_TIMEOUT_MS);
  });

  test('tab 2 types, tab 1 receives the update via DocUpdated', async ({
    page,
    editor,
    browser,
    freshUser,
  }) => {
    const title = `Collab T2toT1 ${randomUUID().slice(0, 8)}`;
    const typed = `t2-${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await editor.assertVisible();

    ctx2 = await browser.newContext();
    const page2 = await ctx2.newPage();
    const editor2 = new EditorAtoms(page2);
    await login(page2, freshUser.email, freshUser.password);
    await openStudy(page2, studyId);
    await editor2.assertVisible();

    // Tab 2 types this time — broadcast goes to Tab 1.
    await editor2.typeAtEnd(typed);

    // Tab 1 must receive the remote steps within the WS SLA.
    await editor.assertContainsWithTimeout(typed, WS_TIMEOUT_MS);
  });

  test('concurrent edits resolve via OT: both tabs eventually contain both strings', async ({
    page,
    editor,
    browser,
    freshUser,
  }) => {
    const title = `Collab Concurrent ${randomUUID().slice(0, 8)}`;
    const textA = `ca-${randomUUID().slice(0, 8)}`;
    const textB = `cb-${randomUUID().slice(0, 8)}`;

    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await editor.assertVisible();

    ctx2 = await browser.newContext();
    const page2 = await ctx2.newPage();
    const editor2 = new EditorAtoms(page2);
    await login(page2, freshUser.email, freshUser.password);
    await openStudy(page2, studyId);
    await editor2.assertVisible();

    // Tab 1 types first. Tab 2 types immediately after, without waiting for
    // Tab 1's DocUpdated broadcast to arrive — so Tab 2 submits steps against
    // a stale version, intentionally triggering DocConflict → rebase →
    // retransmit. We do NOT use Promise.all here because concurrent
    // page.keyboard.type across two pages interleaves individual keystrokes
    // into steps, making the rebased output unpredictable. Sequencing the
    // local input while relying on async WS delivery exercises the same OT
    // path deterministically.
    await editor.typeAtEnd(textA);
    await editor2.typeAtEnd(textB);

    // After OT convergence both tabs must contain both strings.
    await editor.assertContainsWithTimeout(textB, OT_CONFLICT_TIMEOUT_MS);
    await editor2.assertContainsWithTimeout(textA, OT_CONFLICT_TIMEOUT_MS);
  });

  test('second tab opening an already-edited document receives the full document via DocOpened', async ({
    page,
    editor,
    browser,
    freshUser,
  }) => {
    const title = `Collab DocOpen ${randomUUID().slice(0, 8)}`;
    const typed = `open-${randomUUID().slice(0, 8)}`;

    // Tab 1: create study, type, wait for save so the step is persisted.
    await login(page, freshUser.email, freshUser.password);
    const { studyId } = await createStudy(page, title);
    await editor.assertVisible();
    await editor.typeAtEnd(typed);
    await editor.assertSaved();

    // Tab 2: cold open of the same study — must replay steps from DocOpened.
    ctx2 = await browser.newContext();
    const page2 = await ctx2.newPage();
    const editor2 = new EditorAtoms(page2);
    await login(page2, freshUser.email, freshUser.password);
    await openStudy(page2, studyId);
    await editor2.assertVisible();

    // The typed text must be present without any further typing in Tab 2.
    await editor2.assertContains(typed);
  });
});
