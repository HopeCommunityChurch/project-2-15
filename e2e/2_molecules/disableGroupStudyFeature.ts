/**
 * disableGroupStudyFeature molecule — navigates to /profile and disables the
 * GroupStudy feature flag. Leaves the browser on /profile with GroupStudy disabled.
 */

import { Page } from '@playwright/test';
import { goToProfile } from './goToProfile';
import { setFeatureFlag } from './setFeatureFlag';
import type { DisableGroupStudyFeatureResult } from '../types/molecules';

export async function disableGroupStudyFeature(page: Page): Promise<DisableGroupStudyFeatureResult> {
  await goToProfile(page);
  await setFeatureFlag(page, 'GroupStudy', false);
  return { featureName: 'GroupStudy', enabled: false };
}
