/**
 * Public interfaces for every atom class.
 *
 * Each atom class in 1_atoms/ must implement the corresponding interface here.
 * This enforces two things:
 *   1. Every public method has an explicit signature — return type, parameter
 *      types, and arity are all locked down.
 *   2. Renaming or removing a method on a class without updating the interface
 *      (or vice-versa) is a compile error, not a silent runtime break.
 *
 * All atom methods return Promise<void>. Atoms do not return data — that is
 * the job of molecules.
 */

// ── Login page ────────────────────────────────────────────────────────────────

export interface IAuthAtoms {
  fillEmail(email: string): Promise<void>;
  fillPassword(password: string): Promise<void>;
  clickSubmit(): Promise<void>;
  clickNavLogIn(): Promise<void>;
  assertLoggedIn(): Promise<void>;
  assertLoggedOut(): Promise<void>;
}

// ── Nav (present on all pages) ────────────────────────────────────────────────

export interface INavAtoms {
  assertLoggedIn(): Promise<void>;
  assertLoggedOut(): Promise<void>;
}

// ── Studies page ──────────────────────────────────────────────────────────────

export interface IStudiesAtoms {
  clickAddStudy(): Promise<void>;
  assertStudyVisible(title: string): Promise<void>;
  assertStudyNotVisible(title: string): Promise<void>;
  fillNewStudyTitle(title: string): Promise<void>;
  clickCreateStudy(): Promise<void>;
}

// ── History page ──────────────────────────────────────────────────────────────

export interface IHistoryAtoms {
  assertPageLoaded(): Promise<void>;
  assertHistoryGroupsLoaded(): Promise<void>;
  assertDocumentCreatedVisible(): Promise<void>;
  clickFirstGroupPreviewBtn(): Promise<void>;
  assertPreviewVisible(): Promise<void>;
  assertPreviewContains(text: string): Promise<void>;
  clickExpandButton(): Promise<void>;
  assertSubItemsVisible(): Promise<void>;
  clickRestoreButton(): Promise<void>;
  clickHistoryLink(): Promise<void>;
  clickDocumentCreated(): Promise<void>;
  clickFirstSubItem(): Promise<void>;
  clickBackButton(): Promise<void>;
  assertPreviewClosed(): Promise<void>;
}

// ── Editor page ───────────────────────────────────────────────────────────────

export interface IEditorAtoms {
  assertVisible(): Promise<void>;
  typeAtEnd(text: string): Promise<void>;
  assertContains(text: string): Promise<void>;
  assertContainsWithTimeout(text: string, ms: number): Promise<void>;
  assertSaved(): Promise<void>;
  assertTitle(title: string): Promise<void>;
}
