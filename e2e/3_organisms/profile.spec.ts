import { test } from '../fixtures/appPage';
import { login } from '../2_molecules/login';
import { goToProfile } from '../2_molecules/goToProfile';
import { navigateToProfile } from '../2_molecules/navigateToProfile';
import { updateProfile } from '../2_molecules/updateProfile';
import { setFeatureFlag } from '../2_molecules/setFeatureFlag';
import { signOutFromProfile } from '../2_molecules/signOutFromProfile';
import { randomUUID } from 'crypto';

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
});
