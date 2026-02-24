/**
 * Test account credentials used across the E2E suite.
 *
 * All accounts are seeded by fixtures/seed.sql. Auth tests may use the
 * dedicated e2eTest account; feature tests should use main or support accounts.
 *
 * Never put real credentials here — these are isolated test fixtures only.
 *
 * NOTE: Every account listed here must have a matching row in fixtures/seed.sql
 * (user + user_password). If you add, remove, or change a password here, update
 * seed.sql to match — and vice versa.
 */

export const TEST_ACCOUNTS = {
  /** Primary dev account — use for feature tests (editor, studies, etc.) */
  main: { email: 'jhendrie25@gmail.com', password: '2NqL8QBLXF6cPVX' },
  /** Dedicated auth-test account — use for login/logout/signup tests */
  e2eTest: { email: 'e2e-test@example.com', password: 'TestPassword1!' },
  /** Support accounts — use for multi-user / real-time sync tests */
  support1: { email: 'jhendrie25+1@gmail.com', password: '2NqL8QBLXF6cPVX' },
  support2: { email: 'jhendrie25+2@gmail.com', password: '2NqL8QBLXF6cPVX' },
  support3: { email: 'jhendrie25+3@gmail.com', password: '2NqL8QBLXF6cPVX' },
} as const;
