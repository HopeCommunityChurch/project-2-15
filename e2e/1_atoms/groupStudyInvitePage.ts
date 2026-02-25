import { expect, Page } from '@playwright/test';
import type { IGroupStudyInvitePageAtoms } from '../types/atoms';

export class GroupStudyInvitePageAtoms implements IGroupStudyInvitePageAtoms {
  constructor(private page: Page) {}

  /** Asserts the top-level Invites section (`#allShares`) is visible. */
  async assertInvitesSectionVisible() {
    await expect(this.page.locator('#allShares')).toBeVisible();
  }

  /** Asserts the top-level Invites section (`#allShares`) is not present. */
  async assertInvitesSectionNotVisible() {
    await expect(this.page.locator('#allShares')).not.toBeVisible();
  }

  /** Asserts the total number of invite rows (`.row.share` inside `#allShares`). */
  async assertInviteCount(count: number) {
    await expect(this.page.locator('#allShares .row.share')).toHaveCount(count);
  }

  /** Asserts the group study name shown in the first `<div>` of `#share-{token}`. */
  async assertInviteGroupName(token: string, groupName: string) {
    await expect(
      this.page.locator(`#share-${token} div`).first()
    ).toHaveText(groupName);
  }

  /** Asserts the template label shown in the second column of `#share-{token}`. */
  async assertInviteTemplateName(token: string, templateName: string) {
    await expect(
      this.page.locator(`#share-${token} div`).nth(1)
    ).toContainText(templateName);
  }

  /** Selects a document option in the `p-select` dropdown inside `#share-{token}`. Uses JS evaluate because p-select is a custom web component with a closed shadow root. */
  async selectInviteDocument(token: string, value: string) {
    // p-select is a custom element with a closed shadow root — use element.evaluate.
    await this.page.locator(`#share-${token} p-select`).evaluate((el, val) => {
      const opt = Array.from(el.querySelectorAll('option')).find((o: any) => o.value === val);
      if (!opt) throw new Error(`option "${val}" not found in p-select`);
      (el as any).selected(opt, true);
    }, value);
  }

  /** Clicks the Accept button (`button[type="submit"]`) inside `#share-{token}`. */
  async clickAcceptInvite(token: string) {
    await this.page
      .locator(`#share-${token} button[type="submit"]`)
      .click();
  }

  /** Clicks the Reject button (`button.red`) inside `#share-{token}`. */
  async clickRejectInvite(token: string) {
    await this.page.locator(`#share-${token} button.red`).click();
  }

  /** Asserts `#share-{token}` is no longer attached to the DOM. */
  async assertInviteRowGone(token: string) {
    await expect(this.page.locator(`#share-${token}`)).not.toBeAttached();
  }

  /** Asserts the page has navigated to the study editor (URL pattern matches /study/). */
  async assertRedirectedToStudy() {
    await expect(this.page).toHaveURL(/\/study\//);
  }
}
