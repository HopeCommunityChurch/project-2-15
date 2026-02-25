import { expect, Page } from '@playwright/test';
import type { ISignupPageAtoms } from '../types/atoms';

export class SignupPageAtoms implements ISignupPageAtoms {
  constructor(private page: Page) {}

  /** input[placeholder="Jonny Covert"] — name field */
  async fillName(name: string) {
    await this.page.getByPlaceholder('Jonny Covert').fill(name);
  }

  /** input[placeholder="example@example.com"] — email field */
  async fillEmail(email: string) {
    await this.page.getByPlaceholder('example@example.com').fill(email);
  }

  /** input#password — password field */
  async fillPassword(password: string) {
    await this.page.locator('input#password').fill(password);
  }

  /** input#password2 — retype password field */
  async fillRetypePassword(password: string) {
    await this.page.locator('input#password2').fill(password);
  }

  /** input#viewPassword checkbox — toggles password visibility */
  async clickViewPassword() {
    await this.page.getByLabel('View Password').click();
  }

  /** button[type="submit"] — Sign Up button */
  async clickSubmit() {
    await this.page.getByRole('button', { name: 'Sign Up' }).click();
  }

  /** a[href*="/login"] — "Log In" link in "Already have an account?" paragraph */
  async clickLogInLink() {
    await this.page.getByRole('link', { name: 'Log In' }).click();
  }

  /** a[href="/app/"] — logo link in top-left */
  async clickLogoLink() {
    await this.page.getByRole('link', { name: 'Logo' }).click();
  }

  /** Asserts the browser navigated away from /signup after successful signup */
  async assertRedirectedAfterSignup() {
    await this.page.waitForURL(url => !url.pathname.startsWith('/signup'));
  }

  /** ul.errorText li — "Need a name" error message */
  async assertErrorNameRequired() {
    await expect(this.page.getByText('Need a name')).toBeVisible();
  }

  /** ul.errorText li — "Email already taken" error message */
  async assertErrorEmailTaken() {
    await expect(this.page.getByText('Email already taken')).toBeVisible();
  }

  /** ul.errorText li — "Passwords don't match" error message */
  async assertErrorPasswordsMismatch() {
    await expect(this.page.getByText("Passwords don't match")).toBeVisible();
  }

  /** ul.errorText li — "Password needs to be at least 9 characters long" error message */
  async assertErrorPasswordTooShort() {
    await expect(this.page.getByText('Password needs to be at least 9 characters long')).toBeVisible();
  }

  /** ul.errorText li — "Verification failed" ALTCHA captcha error message */
  async assertErrorVerificationFailed() {
    await expect(this.page.getByText('Verification failed')).toBeVisible();
  }

  /** ul.errorText — asserts no validation error list is shown (initial/clean state) */
  async assertNoErrors() {
    await expect(this.page.locator('ul.errorText')).not.toBeVisible();
  }

}
