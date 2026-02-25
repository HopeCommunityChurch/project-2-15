import { expect, Page } from '@playwright/test';
import type { IProfilePageAtoms } from '../types/atoms';

export class ProfilePageAtoms implements IProfilePageAtoms {
  constructor(private page: Page) {}

  // ── Profile form (hx-put /profile) ──────────────────────────────────────────

  /** Fills the Name text input in the profile form. Selector: input[name="name"] */
  async fillName(name: string) {
    await this.page.locator('input[name="name"]').fill(name);
  }

  /** Fills the Email text input in the profile form. Selector: input[name="email"] */
  async fillEmail(email: string) {
    await this.page.locator('input[name="email"]').fill(email);
  }

  /** Clicks the Save button inside the profile form (hx-put /profile). */
  async clickSaveProfile() {
    await this.page
      .locator('form[hx-put]')
      .getByRole('button', { name: 'Save' })
      .click();
  }

  // ── Feature flags form (hx-post /profile/feature) ───────────────────────────

  /**
   * Checks or unchecks a feature-flag checkbox by its feature name.
   * The checkbox id and name attributes equal the feature name string
   * (e.g. "GroupStudy"). Selector: input[type="checkbox"][name="<featureName>"]
   */
  async setFeatureCheckbox(featureName: string, checked: boolean) {
    const cb = this.page.locator(`input[type="checkbox"][name="${featureName}"]`);
    if (checked) {
      await cb.check();
    } else {
      await cb.uncheck();
    }
  }

  /** Clicks the Save button inside the feature flags form (hx-post /profile/feature). */
  async clickSaveFeatures() {
    await this.page
      .locator('form[hx-post]')
      .getByRole('button', { name: 'Save' })
      .click();
  }

  // ── Profile nav dialog ───────────────────────────────────────────────────────

  /** Clicks the profile button in the header to open the profile nav dialog. Selector: button.profileButton */
  async clickProfileButton() {
    await this.page.locator('button.profileButton').click();
  }

  /** Clicks the Sign Out link inside the profile nav dialog. Selector: dialog#profileNav a[href*="signout"] */
  async clickSignOut() {
    await this.page.locator('dialog#profileNav a[href*="signout"]').click();
  }

  /** Clicks the Home link inside the profile nav dialog. Selector: dialog#profileNav a[href="/"] */
  async clickHomeLink() {
    await this.page.locator('dialog#profileNav a[href="/"]').click();
  }

  // ── Assertions ───────────────────────────────────────────────────────────────

  /** Asserts the Name input has the given value. Selector: input[name="name"] */
  async assertNameValue(name: string) {
    await expect(this.page.locator('input[name="name"]')).toHaveValue(name);
  }

  /** Asserts the Email input has the given value. Selector: input[name="email"] */
  async assertEmailValue(email: string) {
    await expect(this.page.locator('input[name="email"]')).toHaveValue(email);
  }

  /**
   * Asserts the profile form "Saved!" notification appears after a successful
   * save. The notification auto-dismisses; this assertion catches it while present.
   * Selector: form[hx-put] p-notification
   */
  async assertProfileSavedNotificationVisible() {
    await expect(
      this.page.locator('form[hx-put] p-notification')
    ).toBeVisible();
  }

  /**
   * Asserts the feature flags form "Saved!" notification appears after a
   * successful save. Selector: form[hx-post] p-notification
   */
  async assertFeaturesSavedNotificationVisible() {
    await expect(
      this.page.locator('form[hx-post] p-notification')
    ).toBeVisible();
  }

  /**
   * Asserts a feature-flag checkbox is checked.
   * Selector: input[type="checkbox"][name="<featureName>"]
   */
  async assertFeatureChecked(featureName: string) {
    await expect(
      this.page.locator(`input[type="checkbox"][name="${featureName}"]`)
    ).toBeChecked();
  }

  /**
   * Asserts a feature-flag checkbox is NOT checked.
   * Selector: input[type="checkbox"][name="<featureName>"]
   */
  async assertFeatureUnchecked(featureName: string) {
    await expect(
      this.page.locator(`input[type="checkbox"][name="${featureName}"]`)
    ).not.toBeChecked();
  }

  /** Asserts the profile nav dialog is open/visible. Selector: dialog#profileNav */
  async assertProfileNavVisible() {
    await expect(this.page.locator('dialog#profileNav')).toBeVisible();
  }
}
