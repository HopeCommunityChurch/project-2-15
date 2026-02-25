/**
 * setFeatureFlag molecule — on the /profile page, toggle a feature-flag
 * checkbox and click Save, then wait for the save response.
 *
 * Leaves the browser on the /profile page with the feature flag updated.
 */

import { Page } from '@playwright/test';
import { ProfilePageAtoms } from '../1_atoms/profilePage';
import type { SetFeatureFlagResult } from '../types/molecules';

export async function setFeatureFlag(
  page: Page,
  featureName: string,
  enabled: boolean,
): Promise<SetFeatureFlagResult> {
  const profile = new ProfilePageAtoms(page);

  await profile.setFeatureCheckbox(featureName, enabled);

  const saveResponse = page.waitForResponse(
    (res) => res.url().includes('/profile/feature') && res.status() === 200,
    { timeout: 10_000 },
  );
  await profile.clickSaveFeatures();
  await saveResponse;

  return { featureName, enabled };
}
