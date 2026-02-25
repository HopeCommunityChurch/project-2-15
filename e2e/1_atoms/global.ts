import { expect, Page } from '@playwright/test';
import type { IGlobalAtoms } from '../types/atoms';

export class GlobalAtoms implements IGlobalAtoms {
  constructor(private page: Page) {}

  /** Asserts the nav bar reflects an authenticated state — profile initials button is visible. */
  async assertLoggedIn() {
    await expect(this.page.locator('button.profileButton')).toBeVisible();
  }

  /** Asserts the nav bar reflects an unauthenticated state — Log In link is visible. */
  async assertLoggedOut() {
    await expect(this.page.getByRole('link', { name: /log in/i })).toBeVisible();
  }
}
