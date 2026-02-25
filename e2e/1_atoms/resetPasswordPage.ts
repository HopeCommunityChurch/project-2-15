import { expect, Page } from '@playwright/test';
import type { IResetPasswordPageAtoms } from '../types/atoms';

export class ResetPasswordPageAtoms implements IResetPasswordPageAtoms {
  constructor(private page: Page) {}

  // ── /resetpassword — email request form ─────────────────────────────────────

  /** Fills the email field. Selector: input[name="email"] */
  async fillEmail(email: string) {
    await this.page.locator('input[name="email"]').fill(email);
  }

  /** Clicks the "Send Password Reset Email" submit button. Selector: button[type="submit"] */
  async clickSendResetEmail() {
    await this.page.getByRole('button', { name: /send password reset email/i }).click();
  }

  /** Asserts the confirmation text appears after submitting the email form.
   *  The backend responds with "Check your email" and HTMX swaps the form. */
  async assertEmailSentConfirmationVisible() {
    await expect(this.page.getByText('Check your email')).toBeVisible();
  }

  // ── /reset_token — new password form ────────────────────────────────────────

  /** Fills the password field. Selector: input[name="password"] */
  async fillNewPassword(password: string) {
    await this.page.locator('input[name="password"]').fill(password);
  }

  /** Fills the retype-password field. Selector: input[name="password2"] */
  async fillRetypePassword(password: string) {
    await this.page.locator('input[name="password2"]').fill(password);
  }

  /** Toggles the "View Password" checkbox. Selector: input#viewPassword */
  async clickViewPassword() {
    await this.page.locator('input#viewPassword').click();
  }

  /** Clicks the "Reset Password" submit button on the token form. Selector: button[type="submit"] */
  async clickSubmitToken() {
    await this.page.getByRole('button', { name: /reset password/i }).click();
  }

  // ── Assertions — token form error states ────────────────────────────────────

  /** Asserts the "token is invalid / expired" error message is visible.
   *  Rendered when errors.tokenIsValid != 1 after form submission. */
  async assertErrorInvalidToken() {
    await expect(
      this.page.getByText(/you took to long try again/i)
    ).toBeVisible();
  }

  /** Asserts the "passwords don't match" error message is visible.
   *  Rendered when errors.passwordsMatch != 1 after form submission. */
  async assertErrorPasswordsMismatch() {
    await expect(
      this.page.getByText(/passwords don't match/i)
    ).toBeVisible();
  }

  /** Asserts the "password too short" error message is visible.
   *  Rendered when errors.passwordLength != 1 after form submission. */
  async assertErrorPasswordTooShort() {
    await expect(
      this.page.getByText(/password needs to be at least 9 characters/i)
    ).toBeVisible();
  }

  /** Asserts that no error messages are visible on the token form. */
  async assertNoErrors() {
    await expect(this.page.locator('ul.errorText')).not.toBeVisible();
  }

  /** Asserts that a successful reset redirects to the studies page. */
  async assertRedirectedAfterReset() {
    await this.page.waitForURL('**/studies');
  }
}
