/**
 * updateProfile molecule — fill in the Name and Email fields on /profile,
 * click Save, and wait for the save response to confirm the update was
 * persisted.
 *
 * Leaves the browser on the /profile page with the updated values.
 */

import { Page } from '@playwright/test';
import { ProfilePageAtoms } from '../1_atoms/profilePage';
import type { UpdateProfileResult } from '../types/molecules';

export async function updateProfile(
  page: Page,
  name: string,
  email: string,
): Promise<UpdateProfileResult> {
  const profile = new ProfilePageAtoms(page);

  await profile.fillName(name);
  await profile.fillEmail(email);

  const saveResponse = page.waitForResponse(
    (res) =>
      res.url().includes('/profile') &&
      !res.url().includes('/profile/feature') &&
      res.request().method() === 'PUT' &&
      res.status() === 200,
    { timeout: 10_000 },
  );
  await profile.clickSaveProfile();
  await saveResponse;

  return { name, email };
}
