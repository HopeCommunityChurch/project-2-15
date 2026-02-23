# Plan: require-email-verification
**Branch:** `require-email-verification`
**Date:** 2026-02-22

---

## Decisions Summary

| Decision | Choice |
|----------|--------|
| Pre-verify state | Account created, login blocked until verified |
| Token expiry | 24 hours |
| Email change | Verify new email before swap; notify old email (informational only) |
| Expired tokens | Account stays in limbo; resend from login page |
| Admin controls | None — fully automated |
| Link click UX | Redirect to login page with success banner |
| Resend location | On login page when unverified user attempts login |
| Resend rate limit | 5-minute server cooldown + client countdown timer |
| Token storage | Separate `user_email_verification` table (plaintext, matching existing patterns) |
| Existing users | Grandfathered as verified (`DEFAULT TRUE`) |

---

## Architecture Overview

### New DB table: `user_email_verification`
```sql
token       text PRIMARY KEY
userId      uuid FK(user)
email       citext          -- email being verified (new email for changes)
expiresAt   timestamptz
usedAt      timestamptz NULL
sentAt      timestamptz     -- for 5-min rate limit on resends
created     timestamptz DEFAULT now()
```

### Modified `user` table
- New column: `emailVerified BOOLEAN NOT NULL DEFAULT TRUE`
- New signups insert with `emailVerified = FALSE`

### Signup flow (new)
1. User submits signup form → account created with `emailVerified = FALSE`
2. Verification email sent (NOT a session cookie)
3. "Check your inbox" page shown (no login)
4. User clicks link → `emailVerified = TRUE`, redirect to login with `?verified=1`
5. Login page shows success banner

### Login flow (new)
1. Password validated successfully
2. Check `user.emailVerified` — if FALSE: re-render login form with error + resend button
3. Resend button triggers `POST /resend_verification`
4. Server checks `sentAt` — if < 5 min ago, returns time remaining
5. Client shows countdown timer ("Resend in 3:42")

### Email change flow (new)
1. User submits new email on profile
2. Old email receives informational notice ("your email is being changed")
3. New email receives verification link
4. Profile shows `<p-notification>`: "Verification email sent to new@example.com"
5. User's current email + `emailVerified` remain unchanged until link clicked
6. On link click: `user.email` updated to new email, token marked used

### Token pattern
- Follows existing `genToken 32` pattern (32 random bytes, base64 encoded)
- Stored plaintext (consistent with password reset, share tokens, session tokens)
- Single active token per user (old tokens invalidated on resend)

---

## Files Affected

### New files
- `backend/migrations/<timestamp>-email-verification.sql`
- `backend/lib/Api/Htmx/EmailVerification.hs`
- `backend/lib/Emails/EmailVerification.hs`
- `backend/lib/Emails/EmailChangeNotification.hs`
- `backend/templates/signup/checkEmail.html`
- `backend/templates/verifyEmail.html`

### Modified files
- `backend/lib/Database.hs` — add `UserT.emailVerified`, add `UserEmailVerificationT` table
- `backend/lib/Types.hs` — add `EmailVerificationToken` type + `genEmailVerificationToken`
- `backend/lib/Entity/User.hs` — update `createUser`, `getPasswordHash`, add verification fns
- `backend/lib/Entity/AuthUser.hs` — update query (UserT now has new column)
- `backend/lib/Api/Htmx/Signup.hs` — don't create session; send verification email; render checkEmail
- `backend/lib/Api/Htmx/Login.hs` — check `emailVerified`; show resend UI
- `backend/lib/Api/Htmx/Profile.hs` — email change triggers verification flow
- `backend/lib/Api/Htmx/Server.hs` — wire new routes
- `backend/templates/login/form.html` — add unverified error + resend UI
- `backend/templates/signup/form.html` — minor: set expectations ("you'll receive a verification email")

---

## Batch 1 — Database Schema + Token Types

### Goals
Add the new DB column, new table, and the Haskell type/generator for verification tokens.

### Checklist
- [x] Create migration file `backend/migrations/2026-02-22T15:00:00.sql`
- [x] In `Database.hs`:
  - Add `emailVerified :: C f Bool` to `UserT` (after `churchId`)
  - Add `UserEmailVerificationT` beam table type with all columns
  - Add `userEmailVerification` field to `Db` record and `database` definition
- [x] In `Types.hs`:
  - Add `EmailVerificationToken'` phantom type and `EmailVerificationToken` newtype
  - Add `genEmailVerificationToken :: MonadIO m => m EmailVerificationToken` using `genToken 32`
- [x] Fix `Entity/User.hs` `createUser` to pass `val_ False` for `emailVerified`
- [x] Run `cabal build` — passes (warnings only)

---

## Batch 2 — Entity Logic + Email Templates

### Goals
Add all backend functions for creating/verifying tokens, and build the two new email templates.

### Checklist
- [x] In `Entity/User.hs`, add:
  - `createVerificationToken` — deletes existing unused tokens for user, inserts new 24h token
  - `verifyEmailToken` — validates token, updates user email + emailVerified, marks used
  - `checkVerificationRateLimit` — returns `Just secondsRemaining` if within 5-min window
  - `updateVerificationSentAt` — updates sentAt for resend rate-limit tracking
- [x] Create `Emails/EmailVerification.hs` — verification email with 24h link
- [x] Create `Emails/EmailChangeNotification.hs` — informational notice to old address
- [x] Register both new modules in `backend.cabal`
- [x] Run `cabal build` — passes

---

## Batch 3 — New Routes: `/verify_email` + `/resend_verification`

### Goals
Create the new HTTP handlers and wire them into the server.

### Checklist
- [ ] Create `Api/Htmx/EmailVerification.hs`:
  - `getVerifyEmail :: (MonadDb env m, HasBaseUrl env, MonadLogger m) => ActionT m ()`
    - Reads `token` query param
    - Calls `verifyEmailToken token`
    - On success: redirect to `/login?verified=1`
    - On failure (invalid/expired): render `templates/verifyEmail.html` with error message
  - `postResendVerification :: (MonadDb env m, HasSmtp env, HasBaseUrl env, MonadLogger m) => ActionT m ()`
    - Reads `email` form param
    - Looks up user by email via `getUnverifiedUserId`, checks they exist and are unverified
    - Calls `checkVerificationRateLimit userId`
    - If rate limited: return HTMX partial with error showing seconds remaining
    - If OK: call `createVerificationToken`, send verification email, return HTMX partial with "Email sent!"
- [x] Create `Api/Htmx/EmailVerification.hs` with `getVerifyEmail` and `postResendVerification`
- [x] Add `getUnverifiedUserId` to `Entity/User.hs`
- [x] Create `templates/verifyEmail.html` — expired/invalid link page (extends base.html)
- [x] Create `templates/signup/checkEmail.html` — post-signup check-inbox partial
- [x] Register `Api.Htmx.EmailVerification` in `backend.cabal`
- [x] Wire `GET /verify_email` and `POST /resend_verification` in `Server.hs`
- [x] Run `cabal build` — passes

---

## Batch 4 — Signup Flow

### Goals
Change signup to NOT auto-login; instead send verification email and show check-your-inbox page.

### Checklist
- [x] In `Api/Htmx/Signup.hs`:
  - Remove `setCookie'` / `HX-Redirect` after user creation
  - Replace welcome email with `createVerificationToken` + `Emails.EmailVerification.mail`
  - Render `signup/checkEmail.html` partial with user's email
  - Add `HasUrl env` constraint; remove now-unused imports
- [x] In `templates/signup/form.html`: add "You'll receive a verification email" note
- [x] Run `cabal build` — passes
- [ ] Test: sign up with a new account → should NOT be logged in → should see "check your inbox"

---

## Batch 5 — Login Flow

### Goals
Block unverified users from logging in; show resend UI with countdown timer.

### Checklist
- [ ] In `Api/Htmx/Login.hs`:
  - Update `getPasswordHash` call sites — handle new return type `(UserId, PasswordHash, Bool)`
  - After password validates successfully:
    - If `emailVerified == True`: proceed as before (create session, redirect)
    - If `emailVerified == False`: call `loginForm` with a new `unverified` flag
  - Add `Unverified` state to the template context (boolean or a separate render path)
- [ ] In `templates/login/form.html`:
  - Add conditional block: when `unverified` is set, show:
    ```html
    <div class="errorText">
      Please verify your email address before logging in.
    </div>
    <div class="resendVerification">
      <form hx-post="{{base}}/resend_verification"
            hx-target="#resend-result"
            hx-swap="innerHTML">
        <input type="hidden" name="email" value="{{email}}">
        <button type="submit" id="resend-btn">Resend verification email</button>
      </form>
      <div id="resend-result"></div>
    </div>
    ```
  - Add countdown timer JavaScript (inline `<script>` in the partial):
    - When resend succeeds, start a 5-minute countdown
    - Disable resend button during countdown, show "Resend in M:SS"
    - On expiry, re-enable button
- [ ] In `templates/login.html` (or `login/form.html`):
  - Add conditional success banner: if `?verified=1` query param present on page load, show `<p-notification time-ms="4000">Email verified! You can now log in.</p-notification>`
  - This can be handled in `Api/Htmx/Login.hs` `getLogin` handler — check query param and pass to template context
- [ ] Update `getLogin` handler (Login.hs) to read `?verified=1` and pass to template
- [ ] Run `cabal build`
- [ ] Test:
  - Try logging in with unverified account → should see error + resend button
  - Click resend → should receive email + button disabled with countdown
  - Click countdown resend again too soon → should show time remaining
  - Verify email → login with success banner

---

## Batch 6 — Profile / Email Change Flow

### Goals
When a user changes their email on the profile page, send verification to new address and notification to old address. Email swap only happens when link is clicked.

### Checklist
- [ ] In `Entity/User.hs`:
  - Add `initiateEmailChange :: MonadDb env m => T.UserId -> T.Email -> T.Email -> m T.EmailVerificationToken`
    - Creates a verification token for `(userId, newEmail)` (same `createVerificationToken`)
    - Returns token (caller sends both emails)
  - Modify `updateUser` (or add `updateUserName`) to only update `name` (not email directly)
    - Email update now only happens via `verifyEmailToken`
- [ ] In `Api/Htmx/Profile.hs`:
  - In `putProfile`: compare submitted email to current `user.email`
  - If email unchanged: update name only (as before)
  - If email changed:
    1. Update name (if changed)
    2. Call `initiateEmailChange userId oldEmail newEmail`
    3. Send `Emails.EmailChangeNotification.mail` to old email
    4. Send `Emails.EmailVerification.mail` to new email (with token)
    5. Render profile form with `<p-notification>`: "Verification email sent to {newEmail}. Your email will update once confirmed."
- [ ] Run `cabal build`
- [ ] Test:
  - Change email on profile → notification shown, both emails sent
  - Old email shows informational notice
  - New email has verification link
  - Clicking link updates user.email and redirects to login

---

## Notes / Risks

- **`AuthUser` query**: Adding `emailVerified` to `UserT` means the beam query in `Entity/AuthUser.hs` needs updating. The `queryEntityBy` projection must include the new field. Verify this compiles.
- **Welcome email**: Currently sent during signup. With verification flow, consider whether to keep it (sends before verification) or replace it with the verification email (one email instead of two). Recommend replacing — send only the verification email on signup.
- **Existing sessions**: Users already logged in are unaffected — `emailVerified` is `TRUE` for all existing users.
- **Token cleanup**: No background job needed — old tokens are deleted when a new one is created (`createVerificationToken` deletes existing unused tokens for that user).
- **`cabal.project` / dependencies**: No new Haskell packages needed — `cryptonite` is already used via `genToken`.
