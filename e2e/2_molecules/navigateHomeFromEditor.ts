/**
 * navigateHomeFromEditor molecule — on the study editor page, click the home
 * logo link in the header and wait for the browser to reach the home page.
 *
 * Leaves the browser on the / home page.
 */

import { Page } from '@playwright/test';
import type { NavigateHomeFromEditorResult } from '../types/molecules';

export async function navigateHomeFromEditor(page: Page): Promise<NavigateHomeFromEditorResult> {
  // The home logo in the editor header is an anchor linking to /.
  await page.locator('header a[href="/"]').first().click();
  await page.waitForURL('**/', { timeout: 10_000 });

  return { url: page.url() };
}
