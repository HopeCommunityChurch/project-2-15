/**
 * getInviteTokenByEmail molecule — navigate to /studies and find the share
 * token for a pending invite sent to the given email address.
 *
 * The invite row is rendered with `id="share-<token>"` inside #allShares.
 * Leaves the browser on the /studies page.
 */

import { Page } from '@playwright/test';
import type { GetInviteTokenByEmailResult } from '../types/molecules';

export async function getInviteTokenByEmail(
  page: Page,
  email: string,
): Promise<GetInviteTokenByEmailResult> {
  await page.goto('/studies');
  await page.waitForURL('**/studies**', { timeout: 10_000 });

  // Wait for the invite section to be present.
  await page.locator('#allShares').waitFor({ state: 'visible', timeout: 10_000 });

  // Find the share row containing the email text and extract the token from
  // the row's id attribute (format: "share-<token>").
  const row = page.locator('#allShares .row.share', { hasText: email });
  const id = await row.getAttribute('id', { timeout: 10_000 });
  if (!id) throw new Error(`getInviteTokenByEmail: no row id found for email: ${email}`);
  const token = id.replace(/^share-/, '');
  if (!token) throw new Error(`getInviteTokenByEmail: could not parse token from id: ${id}`);

  return { token };
}
