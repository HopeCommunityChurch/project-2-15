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

export interface LogoutResult {
  url: string;
}

export interface SignOutFromProfileResult {
  url: string;
}

export interface NavigateToLoginFromHomeResult {
  url: string;
}

// ── Studies ───────────────────────────────────────────────────────────────────

export interface CreateStudyResult {
  title: string;
  url: string;
  studyId: string;
}

export interface DeleteStudyResult {
  url: string;
}

export interface GoToStudiesResult {
  url: string;
}

// ── Editor ────────────────────────────────────────────────────────────────────

export interface OpenStudyResult {
  url: string;
  studyId: string;
}

export interface TypeAndSaveResult {
  text: string;
}

export interface InsertScriptureResult {
  ref: string;
}

export interface ApplyBoldToTextResult {
  text: string;
}

export interface AddSectionResult {
  sectionIndex: number;
}

export interface AddStudyBlockResult {
  blockIndex: number;
}

export interface AddQuestionResult {
  questionIndex: number;
}

// ── Profile ───────────────────────────────────────────────────────────────────

export interface GoToProfileResult {
  url: string;
}

export interface NavigateToProfileResult {
  url: string;
}

export interface UpdateProfileResult {
  name: string;
  email: string;
}

export interface SetFeatureFlagResult {
  featureName: string;
  enabled: boolean;
}

// ── Group Study ───────────────────────────────────────────────────────────────

export interface CreateGroupStudyResult {
  groupName: string;
}

export interface CreateGroupStudyWithInviteResult {
  groupName: string;
  inviteEmail: string;
  permission: 'member' | 'owner';
}

export interface InviteMemberResult {
  email: string;
  permission: string;
}

export interface RemovePendingInviteResult {
  shareToken: string;
}

export interface RemoveMemberResult {
  docId: string;
}

export interface ChangeMemberRoleResult {
  docId: string;
  role: string;
}

export interface GetInviteTokenByEmailResult {
  token: string;
}

export interface AcceptGroupStudyInviteResult {
  token: string;
  url: string;
}

export interface RejectGroupStudyInviteResult {
  token: string;
}
