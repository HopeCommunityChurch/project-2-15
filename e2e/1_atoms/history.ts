import { expect, Page } from '@playwright/test';
import type { IHistoryAtoms } from '../types/atoms';

export class HistoryAtoms implements IHistoryAtoms {
  constructor(private page: Page) {}

  /** `header p` containing "Version History" text on the history page. */
  async assertPageLoaded() {
    await expect(this.page.getByText('Version History')).toBeVisible();
  }

  /** At least one `.historyGroupItem` button is visible in the list. */
  async assertHistoryGroupsLoaded() {
    await expect(this.page.locator('.historyGroupItem').first()).toBeVisible();
  }

  /** `h3.historyDateHeading` with text "Document created" at the bottom of the list. */
  async assertDocumentCreatedVisible() {
    await expect(
      this.page.locator('.historyDateHeading', { hasText: 'Document created' })
    ).toBeVisible();
  }

  /** Clicks the preview button (`.historyGroupPreviewBtn`) on the first history group row. */
  async clickFirstGroupPreviewBtn() {
    await this.page.locator('.historyGroupPreviewBtn').first().click();
  }

  /** `#editorHolder` is visible and contains a non-editable ProseMirror instance. */
  async assertPreviewVisible() {
    await expect(this.page.locator('#editorHolder')).toBeVisible();
    await expect(
      this.page.locator('#editorHolder .ProseMirror[contenteditable="false"]')
    ).toBeVisible();
  }

  /** Preview editor (`#editorHolder .ProseMirror`) contains the given text. */
  async assertPreviewContains(text: string) {
    await expect(
      this.page.locator('#editorHolder .ProseMirror')
    ).toContainText(text);
  }

  /** Clicks the first `.historyExpandBtn` (chevron) to expand sub-items. */
  async clickExpandButton() {
    await this.page.locator('.historyExpandBtn').first().click();
  }

  /** At least one `.historyStepItem` is visible inside the expanded sub-panel. */
  async assertSubItemsVisible() {
    await expect(this.page.locator('.historyStepItem').first()).toBeVisible();
  }

  /** Clicks the "Restore this version" button (`#restoreButton`). */
  async clickRestoreButton() {
    await this.page.locator('#restoreButton').click();
  }

  /** Opens the profile nav dialog (`.profileButton`) then clicks the "Version History" link inside it. */
  async clickHistoryLink() {
    await this.page.locator('.profileButton').click();
    await this.page.getByRole('link', { name: 'Version History' }).click();
  }

  /** Clicks the `.historyFirstVersion` button ("Document created" entry). */
  async clickDocumentCreated() {
    await this.page.locator('.historyFirstVersion').click();
  }
}
