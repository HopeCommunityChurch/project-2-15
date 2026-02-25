import { expect, Page } from '@playwright/test';
import type { IGroupStudyPageAtoms } from '../types/atoms';

/**
 * Atoms for the Group Study modal, which is loaded via HTMX into the
 * `#groupStudy` dialog on the study page (/study/<docId>).
 *
 * The modal has two states:
 *   1. "Create Group Study" — shown when the doc has no group study yet.
 *   2. "Group Study" (manage view) — shown after creation or when the doc is
 *      already part of a group study.
 *
 * All selectors are scoped to `#groupStudy` to avoid conflicts with other
 * page elements.
 */
export class GroupStudyPageAtoms implements IGroupStudyPageAtoms {
  constructor(private page: Page) {}

  /** Scopes all locators to the group study dialog. */
  private dialog() {
    return this.page.locator('#groupStudy');
  }

  // ── Open / close ─────────────────────────────────────────────────────────

  /**
   * Clicks the "Group Study" / "+ Group Study" button in the study page header
   * to open the group study dialog. Selector: `#groupStudyButton`.
   */
  async clickOpenGroupStudy() {
    await this.page.locator('#groupStudyButton').click();
    // Wait for HTMX to load modal content
    await expect(this.dialog()).toBeVisible();
  }

  /**
   * Clicks the close (×) icon inside the group study modal.
   * Selector: `#groupStudy .closeModalIcon`.
   */
  async clickClose() {
    await this.dialog().locator('.closeModalIcon').click();
  }

  // ── Create Group Study form ───────────────────────────────────────────────

  /**
   * Fills the "Study Name" input in the "Create Group Study" form.
   * Selector: `#groupStudy input#createName` (name="name").
   */
  async fillCreateGroupName(name: string) {
    await this.dialog().locator('input#createName').fill(name);
  }

  /**
   * Fills the first invite email input in the "Create Group Study" form.
   * Selector: `#groupStudy #createPeoples input[name="email[]"]`.
   */
  async fillCreateInviteEmail(email: string) {
    await this.dialog().locator('#createPeoples input[name="email[]"]').first().fill(email);
  }

  /**
   * Selects the invite permission in the "Create Group Study" form.
   * Selector: `#groupStudy #createPeoples p-select[name="permission[]"]`.
   * @param permission "member" | "owner"
   */
  async selectCreateInvitePermission(permission: 'member' | 'owner') {
    await this.dialog()
      .locator('#createPeoples p-select[name="permission[]"]')
      .first()
      .selectOption(permission);
  }

  /**
   * Clicks the "Create" submit button in the "Create Group Study" form.
   * Selector: `#groupStudy button[type="submit"].blue` with text "Create".
   */
  async clickCreate() {
    await this.dialog().locator('button[type="submit"].blue', { hasText: 'Create' }).click();
  }

  // ── Manage group name (owner only) ───────────────────────────────────────

  /**
   * Fills the editable group name input in the manage view (owner only).
   * Selector: `#groupStudy input[name="groupName"]`.
   * The name auto-saves after a 1-second debounce (hx-trigger="keyup changed delay:1s").
   */
  async fillGroupName(name: string) {
    await this.dialog().locator('input[name="groupName"]').fill(name);
  }

  /**
   * Asserts the group name input (owner view) has the given value.
   * Selector: `#groupStudy input[name="groupName"]`.
   */
  async assertGroupNameValue(name: string) {
    await expect(this.dialog().locator('input[name="groupName"]')).toHaveValue(name);
  }

  /**
   * Asserts the "Saved!" notification is visible after the group name auto-saves.
   * The notification uses the `<p-notification>` custom element or `.saved` span.
   * Selector: `#groupStudy .saved` (hx-target=".saved" on the name input).
   */
  async assertGroupNameSaved() {
    await expect(this.dialog().locator('.saved')).toContainText('Saved!');
  }

  // ── Invite form (owner only) ──────────────────────────────────────────────

  /**
   * Fills the invite email input in the manage view invite form.
   * Selector: `#groupStudy form.groupStudy-invite-row input[name="email[]"]`.
   */
  async fillInviteEmail(email: string) {
    await this.dialog()
      .locator('form.groupStudy-invite-row input[name="email[]"]')
      .fill(email);
  }

  /**
   * Selects the invite permission in the manage view invite form.
   * Selector: `#groupStudy form.groupStudy-invite-row p-select[name="permission[]"]`.
   * @param permission "member" | "owner"
   */
  async selectInvitePermission(permission: 'member' | 'owner') {
    await this.dialog()
      .locator('form.groupStudy-invite-row p-select[name="permission[]"]')
      .selectOption(permission);
  }

  /**
   * Clicks the "Invite" submit button in the manage view invite form.
   * Selector: `#groupStudy form.groupStudy-invite-row button[type="submit"]`.
   */
  async clickInvite() {
    await this.dialog()
      .locator('form.groupStudy-invite-row button[type="submit"]')
      .click();
  }

  // ── Share row actions (owner only) ───────────────────────────────────────

  /**
   * Clicks the "Resend" button for a pending/expired invite identified by its
   * share token. Selector: `#share-<token> button.ghost-blue`.
   */
  async clickResendInvite(shareToken: string) {
    await this.dialog()
      .locator(`#share-${shareToken} button.ghost-blue`, { hasText: 'Resend' })
      .click();
  }

  /**
   * Clicks the "Remove" button on a pending/expired invite row.
   * Selector: `#share-<token> button.remove-btn`.
   */
  async clickRemoveInvite(shareToken: string) {
    await this.dialog()
      .locator(`#share-${shareToken} button.remove-btn`, { hasText: 'Remove' })
      .click();
  }

  /**
   * Asserts an invite row for the given email address is visible in the
   * "People with access" section. The row uses id `share-<token>`, so we
   * match by email text inside `.share .person-name`.
   */
  async assertInviteVisible(email: string) {
    await expect(
      this.dialog().locator('.share .person-name', { hasText: email }),
    ).toBeVisible();
  }

  /**
   * Asserts an invite row for the given email address is not present.
   */
  async assertInviteNotVisible(email: string) {
    await expect(
      this.dialog().locator('.share .person-name', { hasText: email }),
    ).not.toBeVisible();
  }

  /**
   * Asserts the invite status badge for a share row shows "Invited".
   * Selector: share row containing the email, `.badge-pending` badge.
   */
  async assertInviteStatusPending(email: string) {
    const row = this.dialog().locator('.share', { has: this.page.locator('.person-name', { hasText: email }) });
    await expect(row.locator('.badge-pending')).toBeVisible();
  }

  /**
   * Asserts the invite status badge for a share row shows "Expired" or "Rejected".
   * Selector: share row containing the email, `.badge-error` badge.
   */
  async assertInviteStatusError(email: string) {
    const row = this.dialog().locator('.share', { has: this.page.locator('.person-name', { hasText: email }) });
    await expect(row.locator('.badge-error')).toBeVisible();
  }

  // ── Member row actions (owner only) ──────────────────────────────────────

  /**
   * Changes the ownership role for a member identified by their document ID.
   * Selector: `#member-doc-<docId> p-select[name="ownership"]`.
   * @param docId the UUID of the member's document
   * @param role "member" | "owner"
   */
  async selectMemberOwnership(docId: string, role: 'member' | 'owner') {
    await this.dialog()
      .locator(`#member-doc-${docId} p-select[name="ownership"]`)
      .selectOption(role);
  }

  /**
   * Clicks the "Remove" button for a member row identified by their document ID.
   * Selector: `#member-doc-<docId> button.remove-btn`.
   */
  async clickRemoveMember(docId: string) {
    await this.dialog()
      .locator(`#member-doc-${docId} button.remove-btn`, { hasText: 'Remove' })
      .click();
  }

  /**
   * Asserts a member row for the given display name is visible in the
   * "People with access" section.
   * Selector: `#groupStudy .member .person-name` containing the name text.
   */
  async assertMemberVisible(name: string) {
    await expect(
      this.dialog().locator('.member .person-name', { hasText: name }),
    ).toBeVisible();
  }

  /**
   * Asserts a member row for the given display name is not present.
   */
  async assertMemberNotVisible(name: string) {
    await expect(
      this.dialog().locator('.member .person-name', { hasText: name }),
    ).not.toBeVisible();
  }

  /**
   * Asserts the ownership select for the given document shows the expected role.
   * Selector: `#member-doc-<docId> p-select[name="ownership"]`.
   */
  async assertMemberOwnership(docId: string, role: 'member' | 'owner') {
    await expect(
      this.dialog().locator(`#member-doc-${docId} p-select[name="ownership"]`),
    ).toHaveValue(role);
  }
}
