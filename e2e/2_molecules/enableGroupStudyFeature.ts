/**
 * enableGroupStudyFeature molecule — thin wrapper around setFeatureFlag that
 * enables the GroupStudy feature flag on the /profile page.
 *
 * Assumes the browser is already on the /profile page (the caller should
 * navigate there first via goToProfile). Leaves the browser on /profile with
 * GroupStudy enabled.
 */

import { Page } from '@playwright/test';
import { setFeatureFlag } from './setFeatureFlag';
import type { EnableGroupStudyFeatureResult } from '../types/molecules';

export async function enableGroupStudyFeature(page: Page): Promise<EnableGroupStudyFeatureResult> {
  await setFeatureFlag(page, 'GroupStudy', true);
  return { featureName: 'GroupStudy', enabled: true };
}
