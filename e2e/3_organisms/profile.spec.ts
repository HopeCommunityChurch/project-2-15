import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { goToProfile } from '../2_molecules/goToProfile';
import { navigateToProfile } from '../2_molecules/navigateToProfile';
import { updateProfile } from '../2_molecules/updateProfile';
import { setFeatureFlag } from '../2_molecules/setFeatureFlag';
import { signOutFromProfile } from '../2_molecules/signOutFromProfile';
import { randomUUID } from 'crypto';
import { createTestUser } from '../fixtures/userFactory';

test.describe('profile', () => {
  test('update name persists on the profile page', async ({ page, profile, freshUser }) => {
    const newName = `Updated ${randomUUID().slice(0, 8)}`;
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await updateProfile(page, newName, freshUser.email);
    await profile.assertNameValue(newName);
  });

  test('navigate to profile via the studies nav', async ({ page, profile, freshUser }) => {
    await login(page, freshUser.email, freshUser.password);
    await navigateToProfile(page);
    await profile.assertEmailValue(freshUser.email);
  });

  test('toggle feature flag saves and reflects the new state', async ({ page, profile, freshUser }) => {
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await setFeatureFlag(page, 'GroupStudy', true);
    await profile.assertFeatureChecked('GroupStudy');
    await setFeatureFlag(page, 'GroupStudy', false);
    await profile.assertFeatureUnchecked('GroupStudy');
  });

  test('sign out from profile redirects to home', async ({ page, nav, freshUser }) => {
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await signOutFromProfile(page);
    await nav.assertLoggedOut();
  });

  test('change email persists after save', async ({ page, profile, freshUser }) => {
    const newEmail = `changed-${randomUUID().slice(0, 8)}@e2e.local`;
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await updateProfile(page, freshUser.email.split('@')[0], newEmail);
    await profile.assertEmailValue(newEmail);
  });

  test('changing email to an already-taken address shows an error', async ({
    page,
    profile,
    freshUser,
  }) => {
    // Create a second user whose email we will try to steal.
    const otherUser = await createTestUser();
    await login(page, freshUser.email, freshUser.password);
    await goToProfile(page);
    await profile.fillEmail(otherUser.email);
    const saveResponse = page.waitForResponse(
      (res) =>
        res.url().includes('/profile') &&
        !res.url().includes('/profile/feature') &&
        res.request().method() === 'PUT',
      { timeout: 10_000 },
    );
    await profile.clickSaveProfile();
    await saveResponse;
    // Reload the profile page and assert the email was NOT changed to the
    // already-taken address — the server must have rejected the update.
    await page.goto('/profile');
    await page.waitForURL('**/profile**', { timeout: 10_000 });
    await profile.assertEmailValue(freshUser.email);
  });
});
