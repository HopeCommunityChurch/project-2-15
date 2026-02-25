import { test } from '../fixtures/appPage';
import { GroupStudyInvitePageAtoms, GroupStudyPageAtoms } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { enableGroupStudyFeature } from '../2_molecules/enableGroupStudyFeature';
import { goToStudies } from '../2_molecules/goToStudies';
import { createStudy } from '../2_molecules/createStudy';
import { createGroupStudy } from '../2_molecules/createGroupStudy';
import { createGroupStudyWithInvite } from '../2_molecules/createGroupStudyWithInvite';
import { inviteMember } from '../2_molecules/inviteMember';
import { getInviteTokenByEmail } from '../2_molecules/getInviteTokenByEmail';
import { acceptGroupStudyInvite } from '../2_molecules/acceptGroupStudyInvite';
import { rejectGroupStudyInvite } from '../2_molecules/rejectGroupStudyInvite';
import { openStudy } from '../2_molecules/openStudy';
import { removePendingInvite } from '../2_molecules/removePendingInvite';
import { updateGroupStudyName } from '../2_molecules/updateGroupStudyName';


import { randomUUID } from 'crypto';

test.describe('group study', () => {
  test('create group study', async ({ page, groupStudy, freshUser }) => {
    const title = `GS Create ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    await createStudy(page, title);
    await createGroupStudy(page, groupName);
    await groupStudy.assertGroupNameValue(groupName);
  });

  test('invite a member shows a pending invite row', async ({ page, groupStudy, freshUser, secondaryUser }) => {
    const title = `GS Invite ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    await createStudy(page, title);
    await createGroupStudy(page, groupName);
    await inviteMember(page, secondaryUser.user.email, 'member');
    await groupStudy.assertInviteVisible(secondaryUser.user.email);
    await groupStudy.assertInviteStatusPending(secondaryUser.user.email);
  });

  test('accept invite navigates the invited user to the study editor', async ({ page, freshUser, secondaryUser }) => {
    const title = `GS Accept ${randomUUID().slice(0, 8)}`;
    const title2 = `GS Accept2 ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    // Fetch the token from the owner's manage modal (#share-<token> rows contain the email).
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);
    const { studyId: user2StudyId } = await createStudy(secondaryUser.page, title2);
    await acceptGroupStudyInvite(secondaryUser.page, token, user2StudyId);
    const groupInvite2 = new GroupStudyInvitePageAtoms(secondaryUser.page);
    await groupInvite2.assertRedirectedToStudy();
  });

  test('reject invite removes the invite row for the invitee', async ({ page, freshUser, secondaryUser }) => {
    const title = `GS Reject ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    // Fetch the token from the owner's manage modal (#share-<token> rows contain the email).
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);
    await rejectGroupStudyInvite(secondaryUser.page, token);
    const groupInvite2 = new GroupStudyInvitePageAtoms(secondaryUser.page);
    await groupInvite2.assertInviteRowGone(token);
  });

  test('update group study name reflects new value in the modal input', async ({ page, groupStudy, freshUser }) => {
    const title = `GS Rename ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    const newGroupName = `Renamed Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    await createStudy(page, title);
    await createGroupStudy(page, groupName);
    // The modal is open (createGroupStudy leaves it open with input visible).
    await updateGroupStudyName(page, newGroupName);
    await groupStudy.assertGroupNameValue(newGroupName);
  });

  test('member cannot invite others (invite endpoint rejects non-owners)', async ({
    page,
    freshUser,
    secondaryUser,
  }) => {
    const title = `GS NonOwner ${randomUUID().slice(0, 8)}`;
    const title2 = `GS NonOwner2 ${randomUUID().slice(0, 8)}`;
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

    // Open the member's study and confirm the invite form is not visible.
    await secondaryUser.page.goto(`/study/${memberDocId}`);
    await secondaryUser.page.waitForURL(`**/study/${memberDocId}**`, { timeout: 10_000 });
    await secondaryUser.page.locator('.ProseMirror[contenteditable="true"]').waitFor({ state: 'visible', timeout: 10_000 });
    await secondaryUser.page.locator('#groupStudyButton').click();
    await secondaryUser.page.locator('#groupStudy').waitFor({ state: 'visible', timeout: 10_000 });

    const memberGroupStudy = new GroupStudyPageAtoms(secondaryUser.page);
    await memberGroupStudy.assertInviteFormNotVisible();
  });

  test('remove pending invite removes the row from the modal', async ({ page, groupStudy, freshUser, secondaryUser }) => {
    const title = `GS Remove ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await enableGroupStudyFeature(page);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    // Fetch the token from the owner's manage modal (#share-<token> rows contain the email).
    const { token } = await getInviteTokenByEmail(page, studyId, secondaryUser.user.email);
    await openStudy(page, studyId);
    await removePendingInvite(page, token);
    await groupStudy.assertInviteNotVisible(secondaryUser.user.email);
  });
});
