import { expect, Page } from '@playwright/test';
import type { IAuthAtoms } from '../types/atoms';

export class AuthAtoms implements IAuthAtoms {
  constructor(private page: Page) {}

  /** Fills the email textbox on the login form (placeholder: "example@example.com"). */
  async fillEmail(email: string) {
    await this.page.getByPlaceholder('example@example.com').fill(email);
  }

  /** Fills the password textbox on the login form (input[name="password"]). */
  async fillPassword(password: string) {
    await this.page.locator('input[name="password"]').fill(password);
  }

  /** Clicks the "Log In" submit button on the login form. */
  async clickSubmit() {
    await this.page.getByRole('button', { name: 'Log In' }).click();
  }

  /** Navigates to /login via the "Log In" button in the site nav. */
  async clickNavLogIn() {
    await this.page.getByRole('link', { name: 'Log In' }).click();
  }

  /** Asserts the user is authenticated — URL is the studies page. */
  async assertLoggedIn() {
    await expect(this.page).toHaveURL(/\/studies/);
  }

  /** Asserts the user is unauthenticated — "Log In" button is visible in the nav. */
  async assertLoggedOut() {
    await expect(this.page.getByRole('link', { name: 'Log In' })).toBeVisible();
  }
}
