/**
 * Return types for all molecules.
 *
 * Every molecule returns one of these interfaces — never void, never a raw
 * primitive. Only add a type here when the corresponding molecule exists.
 *
 * Molecules throw on failure (Playwright forces this — waitForURL, expect, etc.
 * all throw). There is no ok/error discriminant; a returned value always means
 * success.
 */

// ── Auth ──────────────────────────────────────────────────────────────────────

export interface LoginResult {
  email: string;
  url: string;
}

// ── Studies ───────────────────────────────────────────────────────────────────

export interface CreateStudyResult {
  title: string;
  url: string;
  studyId: string;
}

// ── History ───────────────────────────────────────────────────────────────────

export interface NavigateToHistoryResult {
  url: string;
  studyId: string;
}

export interface RestoreVersionResult {
  studyId: string;
  url: string;
}
