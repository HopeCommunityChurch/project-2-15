import { expect, Page } from '@playwright/test';
import type { ILoginPageAtoms } from '../types/atoms';

export class LoginPageAtoms implements ILoginPageAtoms {
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

  /** Asserts the browser is on the /login page — login form email input is visible. */
  async assertOnLoginPage() {
    await expect(this.page.getByPlaceholder('example@example.com')).toBeVisible();
  }

  /** Asserts the "Email or password incorrect" error message is shown after a failed login attempt. */
  async assertErrorInvalidCredentials() {
    await expect(this.page.getByText('Email or password incorrect')).toBeVisible();
  }
}
