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

// ── Phase 5 — additional molecules ───────────────────────────────────────────

export interface ApplyItalicToTextResult {
  text: string;
}

export interface ApplyUnderlineToTextResult {
  text: string;
}

export interface ClearFormattingFromTextResult {
  text: string;
}

export interface RenameStudyResult {
  title: string;
}

export interface CancelDeleteStudyResult {
  /** URL stays on the study editor page. */
  url: string;
}

export interface UpdateGroupStudyNameResult {
  groupName: string;
}

export interface ResendInviteResult {
  shareToken: string;
}

export interface OpenGroupStudyModalResult {
  /** Stub — modal is now open. */
  open: true;
}

export interface CloseGroupStudyModalResult {
  /** Stub — modal is now closed. */
  closed: true;
}

export interface AcceptGroupStudyInviteWithExistingDocResult {
  token: string;
  url: string;
}

export interface RequestPasswordResetResult {
  email: string;
}

export interface SignupResult {
  email: string;
  url: string;
}

export interface NavigateToSignupFromHomeResult {
  url: string;
}

export interface EnableGroupStudyFeatureResult {
  featureName: string;
  enabled: true;
}

export interface DisableGroupStudyFeatureResult {
  featureName: string;
  enabled: false;
}

export interface NavigateHomeFromEditorResult {
  url: string;
}

export interface NavigateToProfileFromEditorResult {
  url: string;
}

export interface OpenStudyFromStudiesPageResult {
  url: string;
  studyId: string;
}

export interface CloseNewStudyDialogResult {
  /** Stub — dialog is now closed. */
  closed: true;
}

export interface PreviewScriptureOnlyResult {
  ref: string;
}

export interface UndoChangeResult {
  /** Stub — undo has been applied. */
  applied: true;
}

export interface DeleteSectionResult {
  sectionIndex: number;
}

export interface RedoChangeResult {
  /** Stub — redo has been applied. */
  applied: true;
}
