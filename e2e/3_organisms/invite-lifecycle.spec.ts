import { test } from '../fixtures/appPage';
import { GroupStudyPageAtoms, GroupStudyInvitePageAtoms } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { enableGroupStudyFeature } from '../2_molecules/enableGroupStudyFeature';
import { goToStudies } from '../2_molecules/goToStudies';
import { createStudy } from '../2_molecules/createStudy';

import { createGroupStudyWithInvite } from '../2_molecules/createGroupStudyWithInvite';
import { getInviteTokenByEmail } from '../2_molecules/getInviteTokenByEmail';
import { rejectGroupStudyInvite } from '../2_molecules/rejectGroupStudyInvite';
import { acceptGroupStudyInvite } from '../2_molecules/acceptGroupStudyInvite';
import { removeMember } from '../2_molecules/removeMember';
import { openStudy } from '../2_molecules/openStudy';
import { openGroupStudyModal } from '../2_molecules/openGroupStudyModal';
import { resendInvite } from '../2_molecules/resendInvite';


import { randomUUID } from 'crypto';

test.describe('invite lifecycle', () => {
  test('invite appears in invitee Invites section on studies page @smoke', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Invite ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    // Navigate the invitee to /studies and assert the invite section is visible.
    await secondaryUser.page.goto('/studies');
    await secondaryUser.page.waitForURL('**/studies**', { timeout: 10_000 });
    await secondaryUser.page.locator(`#share-${token}`).waitFor({ state: 'visible', timeout: 10_000 });
    const groupInvite2 = new GroupStudyInvitePageAtoms(secondaryUser.page);
    await groupInvite2.assertInvitesSectionVisible();
  });

  test('rejecting invite removes it from the studies page immediately', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Reject ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    await rejectGroupStudyInvite(secondaryUser.page, token);
    // After rejection the row must be gone (detached from DOM).
    await secondaryUser.page.locator(`#share-${token}`).waitFor({ state: 'detached', timeout: 10_000 });
  });

  test('accepting invite with new document creates a study and joins the group', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Accept ${randomUUID().slice(0, 8)}`;
    const title2 = `IL Accept2 ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    // The invitee creates their own study, then accepts the invite selecting it.
    const { studyId: user2StudyId } = await createStudy(secondaryUser.page, title2);
    await acceptGroupStudyInvite(secondaryUser.page, token, user2StudyId);
    await secondaryUser.page.waitForURL('**/study/**', { timeout: 10_000 });
  });

  test('non-owner member cannot see the invite form in the group study modal', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Member Form ${randomUUID().slice(0, 8)}`;
    const title2 = `IL Member Form2 ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    const { studyId: user2StudyId } = await createStudy(secondaryUser.page, title2);
    const { url } = await acceptGroupStudyInvite(secondaryUser.page, token, user2StudyId);
    // Extract the member's doc ID from the redirect URL (/study/<docId>).
    const memberDocId = url.split('/study/')[1]?.split('?')[0] ?? '';

    // Open the group study modal on the member's page.
    await secondaryUser.page.goto(`/study/${memberDocId}`);
    await secondaryUser.page.waitForURL(`**/study/${memberDocId}**`, { timeout: 10_000 });
    await secondaryUser.page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 10_000 });
    await secondaryUser.page.locator('#groupStudyButton').click();
    // Wait for the modal content to load (HTMX swap).
    await secondaryUser.page.locator('#groupStudy').waitFor({ state: 'visible', timeout: 10_000 });

    const memberGroupStudy = new GroupStudyPageAtoms(secondaryUser.page);
    await memberGroupStudy.assertInviteFormNotVisible();
  });

  test('non-owner member cannot see resend or remove buttons in the group study modal', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Member Btn ${randomUUID().slice(0, 8)}`;
    const title2 = `IL Member Btn2 ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    const { studyId: user2StudyId } = await createStudy(secondaryUser.page, title2);
    const { url } = await acceptGroupStudyInvite(secondaryUser.page, token, user2StudyId);
    const memberDocId = url.split('/study/')[1]?.split('?')[0] ?? '';

    await secondaryUser.page.goto(`/study/${memberDocId}`);
    await secondaryUser.page.waitForURL(`**/study/${memberDocId}**`, { timeout: 10_000 });
    await secondaryUser.page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 10_000 });
    await secondaryUser.page.locator('#groupStudyButton').click();
    await secondaryUser.page.locator('#groupStudy').waitFor({ state: 'visible', timeout: 10_000 });

    const memberGroupStudy = new GroupStudyPageAtoms(secondaryUser.page);
    await memberGroupStudy.assertResendRemoveButtonsNotVisible();
  });

  test('resend invite keeps the row pending and does not remove the invite', async ({
    page,
    groupStudy,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Resend ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    await openStudy(page, studyId);
    await openGroupStudyModal(page);
    await resendInvite(page, token);

    // After resend the invite row must still be visible and still show Pending.
    await groupStudy.assertInviteVisible(secondaryUser.user.email);
    await groupStudy.assertInviteStatusPending(secondaryUser.user.email);
  });

  test('member who accepted invite is visible in owner group study modal', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Member Visible ${randomUUID().slice(0, 8)}`;
    const title2 = `IL Member Visible2 ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    const { studyId: user2StudyId } = await createStudy(secondaryUser.page, title2);
    const { url: acceptedUrl } = await acceptGroupStudyInvite(secondaryUser.page, token, user2StudyId);
    const memberDocId2 = acceptedUrl.split('/study/')[1]?.split('?')[0] ?? '';

    // Owner opens the group study modal and should see the member's row listed.
    await openStudy(page, studyId);
    await openGroupStudyModal(page);
    await page.locator('#groupStudy input[name="groupName"]').waitFor({ state: 'visible', timeout: 10_000 });
    await page.locator(`#member-doc-${memberDocId2}`).waitFor({ state: 'visible', timeout: 10_000 });
  });

  test('removing a member removes them from the group study manage view', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `IL Remove ${randomUUID().slice(0, 8)}`;
    const title2 = `IL Remove2 ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);

    const { studyId: user2StudyId } = await createStudy(secondaryUser.page, title2);
    const { url } = await acceptGroupStudyInvite(secondaryUser.page, token, user2StudyId);
    const memberDocId = url.split('/study/')[1]?.split('?')[0] ?? '';

    // Owner removes the member — the member row should disappear from the modal.
    await openStudy(page, studyId);
    await removeMember(page, memberDocId);

    // After removal the member row (identified by their docId element) is detached.
    // Confirm by asserting the member-doc row is no longer in the DOM.
    await page.locator(`#member-doc-${memberDocId}`).waitFor({ state: 'detached', timeout: 10_000 });
  });
});
