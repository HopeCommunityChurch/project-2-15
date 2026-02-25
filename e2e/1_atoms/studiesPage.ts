import { expect, Page } from '@playwright/test';
import type { IStudiesPageAtoms } from '../types/atoms';

export class StudiesPageAtoms implements IStudiesPageAtoms {
  constructor(private page: Page) {}

  /** Clicks the Add Study / New Study button on the studies list page. */
  async clickAddStudy() {
    await this.page.getByRole('button', { name: /add study|new study/i }).click();
  }

  /** Asserts a study card with the given title is visible in the studies list. */
  async assertStudyVisible(title: string) {
    await expect(this.page.locator('#studiesContent a', { hasText: title })).toBeVisible();
  }

  /** Asserts a study card with the given title is NOT visible in the studies list. */
  async assertStudyNotVisible(title: string) {
    await expect(this.page.locator('#studiesContent a', { hasText: title })).not.toBeVisible();
  }

  /** Fills the title textbox in the new-study dialog. */
  async fillNewStudyTitle(title: string) {
    await this.page.getByRole('dialog').getByRole('textbox').fill(title);
  }

  /** Clicks the "Create" button in the new-study dialog. */
  async clickCreateStudy() {
    await this.page.getByRole('dialog').getByRole('button', { name: 'Create' }).click();
  }

  /**
   * Clicks the close (X) icon on the new-study dialog to dismiss it without
   * creating a study. Targets the img[alt="Close Modal"] inside #newStudy.
   */
  async clickCloseNewStudyDialog() {
    await this.page.locator('#newStudy img[alt="Close Modal"]').click();
  }

  /**
   * Clicks a study card link by its title, navigating to the study editor page.
   * Targets the <a> row whose first child div contains the given title text.
   */
  async clickStudy(title: string) {
    await this.page.locator('#studiesContent a').filter({ hasText: title }).click();
  }

  /**
   * Asserts the total number of rows in the My Studies table equals the given
   * count. Each study is rendered as an <a> element inside .table3.
   */
  async assertStudyCount(count: number) {
    await expect(this.page.locator('#studiesContent .table3 a')).toHaveCount(count);
  }

  /**
   * Asserts that the My Studies table contains no study rows — the empty state.
   */
  async assertEmptyState() {
    await expect(this.page.locator('#studiesContent .table3 a')).toHaveCount(0);
  }

  /**
   * Asserts the group study name shown in the second column of a study row.
   * Pass the study title to identify the row, and the expected group name.
   */
  async assertStudyGroupName(title: string, groupName: string) {
    const row = this.page.locator('#studiesContent a').filter({ hasText: title });
    await expect(row.locator('div').nth(1)).toHaveText(groupName);
  }

  /**
   * Opens the profile nav dialog by clicking the initials button in the header.
   */
  async clickProfileButton() {
    await this.page.locator('#header .profileButton').click();
  }

  /**
   * Clicks the Profile link inside the profile nav dialog.
   * Requires the profile nav to already be open.
   */
  async clickProfileNavProfile() {
    await this.page.locator('#profileNav').getByRole('link', { name: 'Profile' }).click();
  }

  /**
   * Clicks the Sign Out link inside the profile nav dialog.
   * Requires the profile nav to already be open.
   */
  async clickProfileNavSignOut() {
    await this.page.locator('#profileNav').getByRole('link', { name: 'Sign Out' }).click();
  }

}
