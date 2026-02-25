import { test } from '../fixtures/appPage';
import { GroupStudyInvitePageAtoms } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { goToProfile } from '../2_molecules/goToProfile';
import { setFeatureFlag } from '../2_molecules/setFeatureFlag';
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
import { randomUUID } from 'crypto';

test.describe('group study', () => {
  test('create group study', async ({ page, groupStudy, freshUser }) => {
    const title = `GS Create ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await setFeatureFlag(page, 'GroupStudy', true);
    await goToStudies(page);
    await createStudy(page, title);
    await createGroupStudy(page, groupName);
    await groupStudy.assertGroupNameValue(groupName);
  });

  test('invite a member shows a pending invite row', async ({ page, groupStudy, freshUser, secondaryUser }) => {
    const title = `GS Invite ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await setFeatureFlag(page, 'GroupStudy', true);
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
    await goToProfile(page);
    await setFeatureFlag(page, 'GroupStudy', true);
    await goToStudies(page);
    await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, secondaryUser.user.email);
    const { studyId: user2StudyId } = await createStudy(secondaryUser.page, title2);
    await acceptGroupStudyInvite(secondaryUser.page, token, user2StudyId);
    const groupInvite2 = new GroupStudyInvitePageAtoms(secondaryUser.page);
    await groupInvite2.assertRedirectedToStudy();
  });

  test('reject invite removes the invite row for the invitee', async ({ page, freshUser, secondaryUser }) => {
    const title = `GS Reject ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await setFeatureFlag(page, 'GroupStudy', true);
    await goToStudies(page);
    await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, secondaryUser.user.email);
    await rejectGroupStudyInvite(secondaryUser.page, token);
    const groupInvite2 = new GroupStudyInvitePageAtoms(secondaryUser.page);
    await groupInvite2.assertInviteRowGone(token);
  });

  test('remove pending invite removes the row from the modal', async ({ page, groupStudy, freshUser, secondaryUser }) => {
    const title = `GS Remove ${randomUUID().slice(0, 8)}`;
    const groupName = `Group ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await setFeatureFlag(page, 'GroupStudy', true);
    await goToStudies(page);
    const { studyId } = await createStudy(page, title);
    await createGroupStudyWithInvite(page, groupName, secondaryUser.user.email, 'member');
    const { token } = await getInviteTokenByEmail(page, secondaryUser.user.email);
    await openStudy(page, studyId);
    await removePendingInvite(page, token);
    await groupStudy.assertInviteNotVisible(secondaryUser.user.email);
  });
});
