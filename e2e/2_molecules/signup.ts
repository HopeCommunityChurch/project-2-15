/**
 * signup molecule — navigate to /signup, fill all registration fields, submit,
 * and wait for the browser to redirect away from /signup.
 *
 * NOTE: The signup form requires an ALTCHA captcha proof. This molecule works
 * only in environments where ALTCHA validation is bypassed or the challenge has
 * been pre-solved by the test harness. If the captcha check fails the app
 * returns a "Verification failed" error and the redirect will not occur.
 *
 * For most e2e tests, prefer `freshUser` from userFactory.ts (direct DB insert)
 * over this molecule to avoid captcha friction.
 *
 * Leaves the browser on /studies (or the post-signup redirect target) on
 * success.
 */

import { Page } from '@playwright/test';
import { SignupPageAtoms } from '../1_atoms/signupPage';
import type { SignupResult } from '../types/molecules';

export async function signup(
  page: Page,
  name: string,
  email: string,
  password: string,
): Promise<SignupResult> {
  const signupPage = new SignupPageAtoms(page);

  await page.goto('/signup');
  await page.waitForURL('**/signup**', { timeout: 10_000 });

  await signupPage.fillName(name);
  await signupPage.fillEmail(email);
  await signupPage.fillPassword(password);
  await signupPage.fillRetypePassword(password);
  await signupPage.clickSubmit();

  // Wait for navigation away from /signup — successful registration redirects
  // to /studies (or wherever the server sends the user after signup).
  await signupPage.assertRedirectedAfterSignup();

  return { email, url: page.url() };
}
