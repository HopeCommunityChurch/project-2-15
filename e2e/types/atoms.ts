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
  assertOnLoginPage(): Promise<void>;
  assertErrorInvalidCredentials(): Promise<void>;
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
  clickCloseNewStudyDialog(): Promise<void>;
  clickStudy(title: string): Promise<void>;
  assertStudyCount(count: number): Promise<void>;
  assertEmptyState(): Promise<void>;
  assertStudyGroupName(title: string, groupName: string): Promise<void>;
  clickProfileButton(): Promise<void>;
  clickProfileNavProfile(): Promise<void>;
  clickProfileNavSignOut(): Promise<void>;
}

// ── Study page / editor (1_atoms/studyPage.ts) ────────────────────────────────

export interface IStudyPageAtoms {
  assertVisible(): Promise<void>;
  typeAtEnd(text: string): Promise<void>;
  assertContains(text: string): Promise<void>;
  assertContainsWithTimeout(text: string, ms: number): Promise<void>;
  assertSaved(): Promise<void>;
  assertTitle(title: string): Promise<void>;
  // toolbar: formatting
  clickBold(): Promise<void>;
  clickItalic(): Promise<void>;
  clickUnderline(): Promise<void>;
  clickUndo(): Promise<void>;
  clickRedo(): Promise<void>;
  clickOutdent(): Promise<void>;
  clickIndent(): Promise<void>;
  clickClearFormatting(): Promise<void>;
  assertBoldActive(text: string): Promise<void>;
  assertItalicActive(text: string): Promise<void>;
  assertUnderlineActive(text: string): Promise<void>;
  // toolbar: Bible search
  clickInsertScripture(): Promise<void>;
  fillScriptureRef(ref: string): Promise<void>;
  clickScripturePreview(): Promise<void>;
  clickScriptureInsert(): Promise<void>;
  assertScripturePreviewVisible(): Promise<void>;
  // toolbar: study structure
  clickAddStudyBlock(): Promise<void>;
  clickAddQuestion(): Promise<void>;
  // left sidebar
  clickAddSection(): Promise<void>;
  clickCollapseSidebar(): Promise<void>;
  // profile nav / header
  clickProfileMenu(): Promise<void>;
  clickDeleteStudy(): Promise<void>;
  // delete confirmation dialog
  clickConfirmDelete(): Promise<void>;
  clickCancelDelete(): Promise<void>;
  assertConfirmDeleteVisible(): Promise<void>;
  // group study button
  clickGroupStudy(): Promise<void>;
  // structure content assertions
  assertSidebarSectionCountAtLeast(min: number): Promise<void>;
  assertStudyBlockCountAtLeast(min: number): Promise<void>;
  assertQuestionCountAtLeast(min: number): Promise<void>;
}

// ── Home / landing page (1_atoms/homePage.ts) ─────────────────────────────────

export interface IHomePageAtoms {
  clickNavLogIn(): Promise<void>;
  clickNavSignUp(): Promise<void>;
  clickNavTeachings(): Promise<void>;
  clickNavEquipping(): Promise<void>;
  clickNavMessaging(): Promise<void>;
  clickHamburger(): Promise<void>;
  clickMobileNavLogIn(): Promise<void>;
  clickMobileNavSignUp(): Promise<void>;
  clickBeginYourJourney(): Promise<void>;
  clickFooterLogin(): Promise<void>;
  clickFooterContactUs(): Promise<void>;
  assertHeroHeadingVisible(): Promise<void>;
}

// ── Profile page (1_atoms/profilePage.ts) ─────────────────────────────────────

export interface IProfilePageAtoms {
  fillName(name: string): Promise<void>;
  fillEmail(email: string): Promise<void>;
  clickSaveProfile(): Promise<void>;
  setFeatureCheckbox(featureName: string, checked: boolean): Promise<void>;
  clickSaveFeatures(): Promise<void>;
  clickProfileButton(): Promise<void>;
  clickSignOut(): Promise<void>;
  clickHomeLink(): Promise<void>;
  assertNameValue(name: string): Promise<void>;
  assertEmailValue(email: string): Promise<void>;
  assertProfileSavedNotificationVisible(): Promise<void>;
  assertFeaturesSavedNotificationVisible(): Promise<void>;
  assertFeatureChecked(featureName: string): Promise<void>;
  assertFeatureUnchecked(featureName: string): Promise<void>;
  assertProfileNavVisible(): Promise<void>;
}

// ── Signup page (1_atoms/signupPage.ts) ───────────────────────────────────────

export interface ISignupPageAtoms {
  fillName(name: string): Promise<void>;
  fillEmail(email: string): Promise<void>;
  fillPassword(password: string): Promise<void>;
  fillRetypePassword(password: string): Promise<void>;
  clickViewPassword(): Promise<void>;
  clickSubmit(): Promise<void>;
  clickLogInLink(): Promise<void>;
  clickLogoLink(): Promise<void>;
  assertRedirectedAfterSignup(): Promise<void>;
  assertErrorNameRequired(): Promise<void>;
  assertErrorEmailTaken(): Promise<void>;
  assertErrorPasswordsMismatch(): Promise<void>;
  assertErrorPasswordTooShort(): Promise<void>;
  assertErrorVerificationFailed(): Promise<void>;
  assertNoErrors(): Promise<void>;
}

// ── Reset password pages (1_atoms/resetPasswordPage.ts) ───────────────────────

export interface IResetPasswordPageAtoms {
  // /resetpassword — email request form
  fillEmail(email: string): Promise<void>;
  clickSendResetEmail(): Promise<void>;
  assertEmailSentConfirmationVisible(): Promise<void>;
  // /reset_token — new password form
  fillNewPassword(password: string): Promise<void>;
  fillRetypePassword(password: string): Promise<void>;
  clickViewPassword(): Promise<void>;
  clickSubmitToken(): Promise<void>;
  // error state assertions
  assertErrorInvalidToken(): Promise<void>;
  assertErrorPasswordsMismatch(): Promise<void>;
  assertErrorPasswordTooShort(): Promise<void>;
  assertNoErrors(): Promise<void>;
  assertRedirectedAfterReset(): Promise<void>;
}

// ── Group study page — modal loaded into #groupStudy on /study/<docId> ────────
// (1_atoms/groupStudyPage.ts)

export interface IGroupStudyPageAtoms {
  // open / close
  clickOpenGroupStudy(): Promise<void>;
  clickClose(): Promise<void>;
  // create group study form (shown when doc has no group study)
  fillCreateGroupName(name: string): Promise<void>;
  fillCreateInviteEmail(email: string): Promise<void>;
  selectCreateInvitePermission(permission: 'member' | 'owner'): Promise<void>;
  clickCreate(): Promise<void>;
  // manage group name (owner only)
  fillGroupName(name: string): Promise<void>;
  assertGroupNameValue(name: string): Promise<void>;
  assertGroupNameSaved(): Promise<void>;
  // inline invite form (owner only)
  fillInviteEmail(email: string): Promise<void>;
  selectInvitePermission(permission: 'member' | 'owner'): Promise<void>;
  clickInvite(): Promise<void>;
  // share row actions (owner only)
  clickResendInvite(shareToken: string): Promise<void>;
  clickRemoveInvite(shareToken: string): Promise<void>;
  assertInviteVisible(email: string): Promise<void>;
  assertInviteNotVisible(email: string): Promise<void>;
  assertInviteStatusPending(email: string): Promise<void>;
  assertInviteStatusError(email: string): Promise<void>;
  // member row actions (owner only)
  selectMemberOwnership(docId: string, role: 'member' | 'owner'): Promise<void>;
  clickRemoveMember(docId: string): Promise<void>;
  assertMemberVisible(name: string): Promise<void>;
  assertMemberNotVisible(name: string): Promise<void>;
  assertMemberOwnership(docId: string, role: 'member' | 'owner'): Promise<void>;
}

// ── Group study invite page (1_atoms/groupStudyInvitePage.ts) ─────────────────
// The invite section (#allShares) rendered on /studies when the user has
// pending share invites. Accessed directly or via /studies?share_token=<token>.

export interface IGroupStudyInvitePageAtoms {
  // invite section visibility
  assertInvitesSectionVisible(): Promise<void>;
  assertInvitesSectionNotVisible(): Promise<void>;
  assertInviteCount(count: number): Promise<void>;
  // per-invite row content assertions (identified by share token)
  assertInviteGroupName(token: string, groupName: string): Promise<void>;
  assertInviteTemplateName(token: string, templateName: string): Promise<void>;
  // per-invite row actions
  selectInviteDocument(token: string, value: string): Promise<void>;
  clickAcceptInvite(token: string): Promise<void>;
  clickRejectInvite(token: string): Promise<void>;
  // post-action state assertions
  assertInviteRowGone(token: string): Promise<void>;
  assertRedirectedToStudy(): Promise<void>;
}
