/**
 * disableGroupStudyFeature molecule — thin wrapper around setFeatureFlag that
 * disables the GroupStudy feature flag on the /profile page.
 *
 * Assumes the browser is already on the /profile page (the caller should
 * navigate there first via goToProfile). Leaves the browser on /profile with
 * GroupStudy disabled.
 */

import { Page } from '@playwright/test';
import { setFeatureFlag } from './setFeatureFlag';
import type { DisableGroupStudyFeatureResult } from '../types/molecules';

export async function disableGroupStudyFeature(page: Page): Promise<DisableGroupStudyFeatureResult> {
  await setFeatureFlag(page, 'GroupStudy', false);
  return { featureName: 'GroupStudy', enabled: false };
}
