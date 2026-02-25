import { expect, Page } from '@playwright/test';
import type { IStudiesPageAtoms } from '../types/atoms';

export class StudiesPageAtoms implements IStudiesPageAtoms {
  constructor(private page: Page) {}

  /** Clicks the Add Study / New Study button on the studies list page. */
  async clickAddStudy() {
    await this.page.getByRole('button', { name: /add study|new study/i }).click();
  }

  /** Asserts a study card with the given title is visible. */
  async assertStudyVisible(title: string) {
    await expect(this.page.getByText(title)).toBeVisible();
  }

  /** Asserts a study card with the given title is NOT visible. */
  async assertStudyNotVisible(title: string) {
    await expect(this.page.getByText(title)).not.toBeVisible();
  }

  /** Fills the title textbox in the new-study dialog. */
  async fillNewStudyTitle(title: string) {
    await this.page.getByRole('dialog').getByRole('textbox').fill(title);
  }

  /** Clicks the "Create" button in the new-study dialog. */
  async clickCreateStudy() {
    await this.page.getByRole('dialog').getByRole('button', { name: 'Create' }).click();
  }
}
