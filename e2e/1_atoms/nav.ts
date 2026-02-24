import { expect, Page } from '@playwright/test';
import type { INavAtoms } from '../types/atoms';

export class NavAtoms implements INavAtoms {
  constructor(private page: Page) {}

  /** Asserts the nav bar reflects an authenticated state. */
  async assertLoggedIn() {
    await expect(this.page.getByRole('link', { name: /sign out|log out/i })).toBeVisible();
  }

  /** Asserts the nav bar reflects an unauthenticated state. */
  async assertLoggedOut() {
    await expect(this.page.getByRole('link', { name: /sign in|log in/i })).toBeVisible();
  }
}
