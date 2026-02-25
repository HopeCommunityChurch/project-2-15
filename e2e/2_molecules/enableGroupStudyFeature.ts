/**
 * enableGroupStudyFeature molecule — navigates to /profile and enables the
 * GroupStudy feature flag. Leaves the browser on /profile with GroupStudy enabled.
 */

import { Page } from '@playwright/test';
import { goToProfile } from './goToProfile';
import { setFeatureFlag } from './setFeatureFlag';
import type { EnableGroupStudyFeatureResult } from '../types/molecules';

export async function enableGroupStudyFeature(page: Page): Promise<EnableGroupStudyFeatureResult> {
  await goToProfile(page);
  await setFeatureFlag(page, 'GroupStudy', true);
  return { featureName: 'GroupStudy', enabled: true };
}
