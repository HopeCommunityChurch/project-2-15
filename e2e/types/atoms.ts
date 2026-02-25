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

// ── Login page (1_atoms/loginPage.ts) ─────────────────────────────────────────

export interface ILoginPageAtoms {
  fillEmail(email: string): Promise<void>;
  fillPassword(password: string): Promise<void>;
  clickSubmit(): Promise<void>;
  clickNavLogIn(): Promise<void>;
  assertLoggedIn(): Promise<void>;
  assertLoggedOut(): Promise<void>;
}

// ── Global — present on all pages (1_atoms/global.ts) ────────────────────────

export interface IGlobalAtoms {
  assertLoggedIn(): Promise<void>;
  assertLoggedOut(): Promise<void>;
}

// ── Studies page (1_atoms/studiesPage.ts) ─────────────────────────────────────

export interface IStudiesPageAtoms {
  clickAddStudy(): Promise<void>;
  assertStudyVisible(title: string): Promise<void>;
  assertStudyNotVisible(title: string): Promise<void>;
  fillNewStudyTitle(title: string): Promise<void>;
  clickCreateStudy(): Promise<void>;
}

// ── Study page / editor (1_atoms/studyPage.ts) ────────────────────────────────

export interface IStudyPageAtoms {
  assertVisible(): Promise<void>;
  typeAtEnd(text: string): Promise<void>;
  assertContains(text: string): Promise<void>;
  assertContainsWithTimeout(text: string, ms: number): Promise<void>;
  assertSaved(): Promise<void>;
  assertTitle(title: string): Promise<void>;
}
